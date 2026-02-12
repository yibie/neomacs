use neovm_core::TaskHandle;
use neovm_host_abi::{Affinity, LispValue, TaskOptions};
use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread;

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
}

#[derive(Debug)]
pub enum EnqueueError {
    QueueFull,
}

#[derive(Clone, Debug)]
pub struct ScheduledTask {
    pub handle: TaskHandle,
    pub form: LispValue,
    pub opts: TaskOptions,
    pub context: TaskContext,
}

impl ScheduledTask {
    pub fn affinity(&self) -> Affinity {
        self.opts.affinity
    }
}

#[derive(Default)]
struct QueueState {
    queue: VecDeque<ScheduledTask>,
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
}

impl WorkerRuntime {
    pub fn new(config: WorkerConfig) -> Self {
        Self {
            config,
            next_task: AtomicU64::new(1),
            queue: Arc::new(SharedQueue::default()),
        }
    }

    pub fn config(&self) -> WorkerConfig {
        self.config
    }

    pub fn spawn(&self, form: LispValue, opts: TaskOptions) -> Result<TaskHandle, EnqueueError> {
        let handle = TaskHandle(self.next_task.fetch_add(1, Ordering::Relaxed));
        let task = ScheduledTask {
            handle,
            form,
            opts,
            context: TaskContext {
                cancelled: Arc::new(AtomicBool::new(false)),
            },
        };

        let mut state = self.queue.state.lock().expect("worker queue mutex poisoned");
        if state.closed || state.queue.len() >= self.config.queue_capacity {
            return Err(EnqueueError::QueueFull);
        }
        state.queue.push_back(task);
        drop(state);
        self.queue.ready.notify_one();
        Ok(handle)
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
            joins.push(thread::spawn(move || {
                loop {
                    let mut state = queue.state.lock().expect("worker queue mutex poisoned");
                    while state.queue.is_empty() && !state.closed {
                        state = queue
                            .ready
                            .wait(state)
                            .expect("worker queue condvar wait failed");
                    }

                    if state.closed && state.queue.is_empty() {
                        break;
                    }

                    let _task = state.queue.pop_front();
                }
            }));
        }
        joins
    }
}
