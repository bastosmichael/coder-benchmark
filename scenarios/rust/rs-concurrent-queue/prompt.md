
You are a Principal Software Engineer.

I have provided a stub implementation in `src/lib.rs`.
Your task is to implement a robust, generic `TaskQueue` that handles concurrency management, rate limiting, and retries.

```rust
use std::sync::{Arc, Mutex};
use std::time::Duration;
use std::sync::mpsc::Receiver;

pub struct TaskOptions {
    pub concurrency: usize,
    pub rate_limit: Option<RateLimit>,
    pub max_retries: usize,
}

pub struct RateLimit {
    pub tokens: usize,
    pub interval: Duration,
}

pub struct TaskQueue<T> {
    // fields
}

impl<T: Send + 'static> TaskQueue<T> {
    pub fn new(opts: TaskOptions) -> Self {
        TaskQueue { }
    }

    /// Adds a task to the queue.
    /// Returns a Receiver that will eventually receive the result.
    pub fn add<F>(&self, task: F) -> Receiver<Result<T, String>>
    where
        F: Fn() -> Result<T, String> + Send + Sync + 'static,
    {
        // implementation
        let (tx, rx) = std::sync::mpsc::channel();
        rx
    }

    pub fn status(&self) -> Status {
        Status { pending: 0, running: 0, completed: 0, failed: 0 }
    }
}

pub struct Status {
    pub pending: usize,
    pub running: usize,
    pub completed: usize,
    pub failed: usize,
}
```

Requirements:
1. **Concurrency Control**: At most `opts.concurrency` tasks should run simultaneously in threads.
2. **Rate Limiting**: If `rate_limit` is provided, ensure tasks are started respecting the `tokens` per `interval` limit.
3. **Retries**: If a task fails (returns Err), it should be retried up to `max_retries` times with exponential backoff.
4. **Ordering**: Tasks should generally run in First-In-First-Out (FIFO) order, respecting concurrency limits.
5. **No External Libraries**: Use only native Rust `std`.
6. **Thread Safety**: The queue must be safe for concurrent use (e.g. `Arc<Mutex<...>>` or channels).

Correctness is critical. Deadlocks or race conditions are failures.
Output the full content of `src/lib.rs` in a single markdown code block.
