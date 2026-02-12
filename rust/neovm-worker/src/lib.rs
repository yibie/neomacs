use neovm_core::{TaskHandle, TaskScheduler, TaskStatus};
use neovm_host_abi::{Affinity, LispValue, SelectOp, SelectResult, Signal, TaskError, TaskOptions};
use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex, RwLock};
use std::thread;
use std::time::{Duration, Instant};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct WorkerConfig {
    pub threads: usize,
    pub queue_capacity: usize,
}

impl Default for WorkerConfig {
    fn default() -> Self {
        Self {
            threads: 1,
            queue_capacity: 1024,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TaskContext {
    cancelled: Arc<AtomicBool>,
}

impl TaskContext {
    pub fn is_cancelled(&self) -> bool {
        self.cancelled.load(Ordering::Acquire)
    }

    pub fn cancel(&self) {
        self.cancelled.store(true, Ordering::Release);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EnqueueError {
    Closed,
    QueueFull,
    MainAffinityUnsupported,
}

#[derive(Debug)]
struct TaskEntry {
    form: LispValue,
    opts: TaskOptions,
    context: TaskContext,
    status: Mutex<TaskStatus>,
}

impl TaskEntry {
    fn new(form: LispValue, opts: TaskOptions) -> Self {
        Self {
            form,
            opts,
            context: TaskContext {
                cancelled: Arc::new(AtomicBool::new(false)),
            },
            status: Mutex::new(TaskStatus::Queued),
        }
    }

    fn set_status(&self, status: TaskStatus) {
        let mut slot = self.status.lock().expect("task status mutex poisoned");
        *slot = status;
    }

    fn status(&self) -> TaskStatus {
        *self.status.lock().expect("task status mutex poisoned")
    }
}

#[derive(Default)]
struct QueueState {
    queue: VecDeque<TaskHandle>,
    closed: bool,
}

#[derive(Default)]
struct SharedQueue {
    state: Mutex<QueueState>,
    ready: Condvar,
}

pub struct WorkerRuntime {
    config: WorkerConfig,
    next_task: AtomicU64,
    queue: Arc<SharedQueue>,
    tasks: Arc<RwLock<HashMap<u64, Arc<TaskEntry>>>>,
}

impl WorkerRuntime {
    pub fn new(config: WorkerConfig) -> Self {
        Self {
            config,
            next_task: AtomicU64::new(1),
            queue: Arc::new(SharedQueue::default()),
            tasks: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub fn config(&self) -> WorkerConfig {
        self.config
    }

    pub fn spawn(&self, form: LispValue, opts: TaskOptions) -> Result<TaskHandle, EnqueueError> {
        if opts.affinity == Affinity::MainOnly {
            return Err(EnqueueError::MainAffinityUnsupported);
        }

        let handle = TaskHandle(self.next_task.fetch_add(1, Ordering::Relaxed));
        let task = Arc::new(TaskEntry::new(form, opts));

        {
            let mut state = self.queue.state.lock().expect("worker queue mutex poisoned");
            if state.closed {
                return Err(EnqueueError::Closed);
            }
            if state.queue.len() >= self.config.queue_capacity {
                return Err(EnqueueError::QueueFull);
            }

            // Register task before releasing the queue lock so workers cannot
            // observe a handle that has no task entry yet.
            let mut tasks = self.tasks.write().expect("tasks map rwlock poisoned");
            tasks.insert(handle.0, task);
            state.queue.push_back(handle);
        }

        self.queue.ready.notify_one();
        Ok(handle)
    }

    pub fn cancel(&self, handle: TaskHandle) -> bool {
        let task = {
            let tasks = self.tasks.read().expect("tasks map rwlock poisoned");
            tasks.get(&handle.0).cloned()
        };

        let Some(task) = task else {
            return false;
        };

        task.context.cancel();

        if task.status() == TaskStatus::Queued {
            task.set_status(TaskStatus::Cancelled);
        }
        true
    }

    pub fn task_status(&self, handle: TaskHandle) -> Option<TaskStatus> {
        let tasks = self.tasks.read().expect("tasks map rwlock poisoned");
        tasks.get(&handle.0).map(|entry| entry.status())
    }

    pub fn close(&self) {
        let mut state = self.queue.state.lock().expect("worker queue mutex poisoned");
        state.closed = true;
        drop(state);
        self.queue.ready.notify_all();
    }

    pub fn start_dummy_workers(&self) -> Vec<thread::JoinHandle<()>> {
        let mut joins = Vec::with_capacity(self.config.threads);
        for _ in 0..self.config.threads {
            let queue = Arc::clone(&self.queue);
            let tasks = Arc::clone(&self.tasks);
            joins.push(thread::spawn(move || loop {
                let handle = {
                    let mut state = queue.state.lock().expect("worker queue mutex poisoned");
                    while state.queue.is_empty() && !state.closed {
                        state = queue
                            .ready
                            .wait(state)
                            .expect("worker queue condvar wait failed");
                    }

                    if state.closed && state.queue.is_empty() {
                        return;
                    }

                    state.queue.pop_front()
                };

                let Some(handle) = handle else {
                    continue;
                };

                let task = {
                    let tasks = tasks.read().expect("tasks map rwlock poisoned");
                    tasks.get(&handle.0).cloned()
                };

                let Some(task) = task else {
                    continue;
                };

                if task.context.is_cancelled() || task.status() == TaskStatus::Cancelled {
                    task.set_status(TaskStatus::Cancelled);
                    continue;
                }

                task.set_status(TaskStatus::Running);

                // Placeholder execution path: a real runtime would evaluate task.form
                // inside an isolate and write the result to a completion channel.
                let _ = task.form.bytes.len();
                let _ = task.opts.name.as_deref();

                if task.context.is_cancelled() {
                    task.set_status(TaskStatus::Cancelled);
                } else {
                    task.set_status(TaskStatus::Completed);
                }
            }));
        }
        joins
    }

    fn task_await_result(
        &self,
        handle: TaskHandle,
        timeout: Option<Duration>,
    ) -> Result<LispValue, TaskError> {
        fn map_status(status: Option<TaskStatus>) -> Result<LispValue, TaskError> {
            match status {
                Some(TaskStatus::Completed) => Ok(LispValue::default()),
                Some(TaskStatus::Cancelled) => Err(TaskError::Cancelled),
                Some(TaskStatus::Queued) | Some(TaskStatus::Running) | None => {
                    Err(TaskError::TimedOut)
                }
            }
        }

        let Some(timeout) = timeout else {
            return map_status(self.task_status(handle));
        };

        let deadline = Instant::now() + timeout;
        loop {
            match self.task_status(handle) {
                Some(TaskStatus::Completed) => return Ok(LispValue::default()),
                Some(TaskStatus::Cancelled) => return Err(TaskError::Cancelled),
                _ => {
                    if Instant::now() >= deadline {
                        return Err(TaskError::TimedOut);
                    }
                    thread::yield_now();
                }
            }
        }
    }
}

fn enqueue_error_to_signal(err: EnqueueError) -> Signal {
    match err {
        EnqueueError::Closed => Signal {
            symbol: "task-queue-closed".to_string(),
            data: None,
        },
        EnqueueError::QueueFull => Signal {
            symbol: "task-queue-full".to_string(),
            data: None,
        },
        EnqueueError::MainAffinityUnsupported => Signal {
            symbol: "task-main-affinity-unsupported".to_string(),
            data: None,
        },
    }
}

impl TaskScheduler for WorkerRuntime {
    fn spawn_task(&self, form: LispValue, opts: TaskOptions) -> Result<TaskHandle, Signal> {
        self.spawn(form, opts).map_err(enqueue_error_to_signal)
    }

    fn task_cancel(&self, handle: TaskHandle) -> bool {
        self.cancel(handle)
    }

    fn task_status(&self, handle: TaskHandle) -> Option<TaskStatus> {
        WorkerRuntime::task_status(self, handle)
    }

    fn task_await(
        &self,
        handle: TaskHandle,
        timeout: Option<Duration>,
    ) -> Result<LispValue, TaskError> {
        self.task_await_result(handle, timeout)
    }

    fn select(&self, _ops: &[SelectOp], _timeout: Option<Duration>) -> SelectResult {
        SelectResult::TimedOut
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn spawn_and_cancel_task() {
        let rt = WorkerRuntime::new(WorkerConfig::default());
        let task = rt
            .spawn(LispValue::default(), TaskOptions::default())
            .expect("task should enqueue");
        assert_eq!(rt.task_status(task), Some(TaskStatus::Queued));
        assert!(rt.cancel(task));
        assert_eq!(rt.task_status(task), Some(TaskStatus::Cancelled));
    }

    #[test]
    fn reject_main_only_task_on_worker_runtime() {
        let rt = WorkerRuntime::new(WorkerConfig::default());
        let opts = TaskOptions {
            affinity: Affinity::MainOnly,
            ..TaskOptions::default()
        };
        let err = rt.spawn(LispValue::default(), opts).expect_err("must reject");
        assert!(matches!(err, EnqueueError::MainAffinityUnsupported));
    }

    #[test]
    fn scheduler_trait_maps_queue_full_to_signal() {
        let rt = WorkerRuntime::new(WorkerConfig {
            threads: 1,
            queue_capacity: 0,
        });
        let err = TaskScheduler::spawn_task(&rt, LispValue::default(), TaskOptions::default())
            .expect_err("must map queue pressure to signal");
        assert_eq!(err.symbol, "task-queue-full");
    }

    #[test]
    fn dummy_worker_marks_task_completed() {
        let rt = WorkerRuntime::new(WorkerConfig {
            threads: 1,
            queue_capacity: 16,
        });
        let workers = rt.start_dummy_workers();
        let task = rt
            .spawn(LispValue::default(), TaskOptions::default())
            .expect("task should enqueue");

        let mut completed = false;
        for _ in 0..100 {
            if rt.task_status(task) == Some(TaskStatus::Completed) {
                completed = true;
                break;
            }
            thread::sleep(Duration::from_millis(1));
        }

        rt.close();
        for worker in workers {
            worker.join().expect("worker thread should join");
        }

        assert!(completed, "task should complete on dummy worker");
    }

    #[test]
    fn scheduler_await_reports_cancelled_task() {
        let rt = WorkerRuntime::new(WorkerConfig::default());
        let task = rt
            .spawn(LispValue::default(), TaskOptions::default())
            .expect("task should enqueue");
        assert!(rt.cancel(task));

        let err = TaskScheduler::task_await(&rt, task, None).expect_err("task should cancel");
        assert!(matches!(err, TaskError::Cancelled));
    }
}
