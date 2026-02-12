use neovm_core::{TaskHandle, TaskScheduler, TaskStatus};
use neovm_host_abi::{
    Affinity, LispValue, SelectOp, SelectResult, Signal, TaskError, TaskOptions, TaskPriority,
};
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

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct RuntimeStats {
    pub enqueued: u64,
    pub dequeued: u64,
    pub completed: u64,
    pub cancelled: u64,
    pub rejected_closed: u64,
    pub rejected_full: u64,
    pub rejected_affinity: u64,
}

#[derive(Default)]
struct RuntimeMetrics {
    enqueued: AtomicU64,
    dequeued: AtomicU64,
    completed: AtomicU64,
    cancelled: AtomicU64,
    rejected_closed: AtomicU64,
    rejected_full: AtomicU64,
    rejected_affinity: AtomicU64,
}

impl RuntimeMetrics {
    fn snapshot(&self) -> RuntimeStats {
        RuntimeStats {
            enqueued: self.enqueued.load(Ordering::Relaxed),
            dequeued: self.dequeued.load(Ordering::Relaxed),
            completed: self.completed.load(Ordering::Relaxed),
            cancelled: self.cancelled.load(Ordering::Relaxed),
            rejected_closed: self.rejected_closed.load(Ordering::Relaxed),
            rejected_full: self.rejected_full.load(Ordering::Relaxed),
            rejected_affinity: self.rejected_affinity.load(Ordering::Relaxed),
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

    fn status(&self) -> TaskStatus {
        *self.status.lock().expect("task status mutex poisoned")
    }

    fn mark_running(&self) -> bool {
        let mut status = self.status.lock().expect("task status mutex poisoned");
        if *status == TaskStatus::Queued {
            *status = TaskStatus::Running;
            true
        } else {
            false
        }
    }

    fn mark_cancelled(&self) -> bool {
        let mut status = self.status.lock().expect("task status mutex poisoned");
        match *status {
            TaskStatus::Completed | TaskStatus::Cancelled => false,
            TaskStatus::Queued | TaskStatus::Running => {
                *status = TaskStatus::Cancelled;
                true
            }
        }
    }

    fn mark_completed(&self) -> bool {
        let mut status = self.status.lock().expect("task status mutex poisoned");
        match *status {
            TaskStatus::Cancelled | TaskStatus::Completed => false,
            TaskStatus::Queued | TaskStatus::Running => {
                *status = TaskStatus::Completed;
                true
            }
        }
    }
}

#[derive(Default)]
struct QueueState {
    interactive: VecDeque<TaskHandle>,
    default: VecDeque<TaskHandle>,
    background: VecDeque<TaskHandle>,
    closed: bool,
}

impl QueueState {
    fn len(&self) -> usize {
        self.interactive.len() + self.default.len() + self.background.len()
    }

    fn is_empty(&self) -> bool {
        self.interactive.is_empty() && self.default.is_empty() && self.background.is_empty()
    }

    fn push(&mut self, handle: TaskHandle, priority: TaskPriority) {
        match priority {
            TaskPriority::Interactive => self.interactive.push_back(handle),
            TaskPriority::Default => self.default.push_back(handle),
            TaskPriority::Background => self.background.push_back(handle),
        }
    }

    fn pop(&mut self) -> Option<TaskHandle> {
        self.interactive
            .pop_front()
            .or_else(|| self.default.pop_front())
            .or_else(|| self.background.pop_front())
    }
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
    metrics: Arc<RuntimeMetrics>,
}

impl WorkerRuntime {
    pub fn new(config: WorkerConfig) -> Self {
        Self {
            config,
            next_task: AtomicU64::new(1),
            queue: Arc::new(SharedQueue::default()),
            tasks: Arc::new(RwLock::new(HashMap::new())),
            metrics: Arc::new(RuntimeMetrics::default()),
        }
    }

    pub fn config(&self) -> WorkerConfig {
        self.config
    }

    pub fn stats(&self) -> RuntimeStats {
        self.metrics.snapshot()
    }

    pub fn spawn(&self, form: LispValue, opts: TaskOptions) -> Result<TaskHandle, EnqueueError> {
        if opts.affinity == Affinity::MainOnly {
            self.metrics.rejected_affinity.fetch_add(1, Ordering::Relaxed);
            return Err(EnqueueError::MainAffinityUnsupported);
        }

        let handle = TaskHandle(self.next_task.fetch_add(1, Ordering::Relaxed));
        let priority = opts.priority;
        let task = Arc::new(TaskEntry::new(form, opts));

        {
            let mut state = self.queue.state.lock().expect("worker queue mutex poisoned");
            if state.closed {
                self.metrics.rejected_closed.fetch_add(1, Ordering::Relaxed);
                return Err(EnqueueError::Closed);
            }
            if state.len() >= self.config.queue_capacity {
                self.metrics.rejected_full.fetch_add(1, Ordering::Relaxed);
                return Err(EnqueueError::QueueFull);
            }

            // Register task before releasing the queue lock so workers cannot
            // observe a handle that has no task entry yet.
            let mut tasks = self.tasks.write().expect("tasks map rwlock poisoned");
            tasks.insert(handle.0, task);
            state.push(handle, priority);
        }

        self.metrics.enqueued.fetch_add(1, Ordering::Relaxed);
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
        if task.mark_cancelled() {
            self.metrics.cancelled.fetch_add(1, Ordering::Relaxed);
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
            let metrics = Arc::clone(&self.metrics);
            joins.push(thread::spawn(move || loop {
                let handle = {
                    let mut state = queue.state.lock().expect("worker queue mutex poisoned");
                    while state.is_empty() && !state.closed {
                        state = queue
                            .ready
                            .wait(state)
                            .expect("worker queue condvar wait failed");
                    }

                    if state.closed && state.is_empty() {
                        return;
                    }

                    state.pop()
                };

                let Some(handle) = handle else {
                    continue;
                };
                metrics.dequeued.fetch_add(1, Ordering::Relaxed);

                let task = {
                    let tasks = tasks.read().expect("tasks map rwlock poisoned");
                    tasks.get(&handle.0).cloned()
                };

                let Some(task) = task else {
                    continue;
                };

                if task.context.is_cancelled() {
                    if task.mark_cancelled() {
                        metrics.cancelled.fetch_add(1, Ordering::Relaxed);
                    }
                    continue;
                }

                if !task.mark_running() {
                    continue;
                }

                // Placeholder execution path: a real runtime would evaluate task.form
                // inside an isolate and write the result to a completion channel.
                let _ = task.form.bytes.len();
                let _ = task.opts.name.as_deref();

                if task.context.is_cancelled() {
                    if task.mark_cancelled() {
                        metrics.cancelled.fetch_add(1, Ordering::Relaxed);
                    }
                } else if task.mark_completed() {
                    metrics.completed.fetch_add(1, Ordering::Relaxed);
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
    fn queue_state_prioritizes_interactive() {
        let mut queue = QueueState::default();
        queue.push(TaskHandle(1), TaskPriority::Background);
        queue.push(TaskHandle(2), TaskPriority::Default);
        queue.push(TaskHandle(3), TaskPriority::Interactive);

        assert_eq!(queue.pop(), Some(TaskHandle(3)));
        assert_eq!(queue.pop(), Some(TaskHandle(2)));
        assert_eq!(queue.pop(), Some(TaskHandle(1)));
        assert_eq!(queue.pop(), None);
    }

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
        let stats = rt.stats();
        assert_eq!(stats.enqueued, 1);
        assert_eq!(stats.dequeued, 1);
        assert_eq!(stats.completed, 1);
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

    #[test]
    fn runtime_stats_track_rejections_and_cancellation() {
        let rt = WorkerRuntime::new(WorkerConfig {
            threads: 0,
            queue_capacity: 1,
        });

        let queued = rt
            .spawn(LispValue::default(), TaskOptions::default())
            .expect("first task should enqueue");

        rt.spawn(LispValue::default(), TaskOptions::default())
            .expect_err("second task should hit queue limit");

        rt.spawn(
            LispValue::default(),
            TaskOptions {
                affinity: Affinity::MainOnly,
                ..TaskOptions::default()
            },
        )
        .expect_err("main-only task should be rejected");

        rt.close();
        rt.spawn(LispValue::default(), TaskOptions::default())
            .expect_err("closed runtime should reject new tasks");

        assert!(rt.cancel(queued));

        let stats = rt.stats();
        assert_eq!(stats.enqueued, 1);
        assert_eq!(stats.rejected_full, 1);
        assert_eq!(stats.rejected_affinity, 1);
        assert_eq!(stats.rejected_closed, 1);
        assert_eq!(stats.cancelled, 1);
    }
}
