use neovm_host_abi::{
    HostAbi, LispValue, SelectOp, SelectResult, Signal, TaskError, TaskOptions,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TaskHandle(pub u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TaskStatus {
    Queued,
    Running,
    Completed,
    Cancelled,
}

/// Core VM shell that will later host the evaluator, GC, and JIT entry points.
///
/// This intentionally keeps only API-level behavior for now.
pub struct Vm<H: HostAbi> {
    host: H,
    next_task_id: u64,
}

impl<H: HostAbi> Vm<H> {
    pub fn new(host: H) -> Self {
        Self {
            host,
            next_task_id: 1,
        }
    }

    pub fn host(&self) -> &H {
        &self.host
    }

    pub fn host_mut(&mut self) -> &mut H {
        &mut self.host
    }

    pub fn spawn_task(
        &mut self,
        _form: LispValue,
        _opts: TaskOptions,
    ) -> Result<TaskHandle, Signal> {
        // Placeholder task allocation. Scheduler wiring happens in neovm-worker.
        let handle = TaskHandle(self.next_task_id);
        self.next_task_id = self.next_task_id.saturating_add(1);
        Ok(handle)
    }

    pub fn task_await(
        &mut self,
        _handle: TaskHandle,
        _timeout: Option<std::time::Duration>,
    ) -> Result<LispValue, TaskError> {
        Err(TaskError::TimedOut)
    }

    pub fn task_cancel(&mut self, _handle: TaskHandle) -> bool {
        false
    }

    pub fn select(
        &mut self,
        _ops: &[SelectOp],
        _timeout: Option<std::time::Duration>,
    ) -> SelectResult {
        SelectResult::TimedOut
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SchedulerConfig {
    pub worker_threads: usize,
}

impl Default for SchedulerConfig {
    fn default() -> Self {
        Self { worker_threads: 1 }
    }
}
