use std::sync::mpsc::Receiver;
use std::time::Duration;

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
}

pub struct Status {
    pub pending: usize,
    pub running: usize,
    pub completed: usize,
    pub failed: usize,
}

impl<T: Send + 'static> TaskQueue<T> {
    pub fn new(opts: TaskOptions) -> Self {
        TaskQueue {}
    }

    pub fn add<F>(&self, task: F) -> Receiver<Result<T, String>>
    where
        F: Fn() -> Result<T, String> + Send + Sync + 'static,
    {
        let (tx, rx) = std::sync::mpsc::channel();
        // tx.send(Err("Not implemented".to_string())).ok(); 
        // We return rx. If implementation does nothing, rx.recv() blocks/drops.
        rx
    }

    pub fn status(&self) -> Status {
        Status { pending: 0, running: 0, completed: 0, failed: 0 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Arc, Mutex, atomic::{AtomicUsize, Ordering}};
    use std::thread;

    #[test]
    fn test_basic_execution() {
        let q = TaskQueue::<String>::new(TaskOptions { concurrency: 1, rate_limit: None, max_retries: 0 });
        let rx = q.add(|| Ok("ok".to_string()));
        
        match rx.recv_timeout(Duration::from_secs(2)) {
            Ok(Ok(val)) => assert_eq!(val, "ok"),
            Ok(Err(e)) => panic!("Task failed: {}", e),
            Err(_) => panic!("Timeout - task likely not started"),
        }
    }

    #[test]
    fn test_retries() {
        // Need Arc to share state count between closure invocations
        let attempts = Arc::new(AtomicUsize::new(0));
        let attempts_clone = attempts.clone();

        let q = TaskQueue::<String>::new(TaskOptions { concurrency: 1, rate_limit: None, max_retries: 2 });
        
        let rx = q.add(move || {
            let count = attempts_clone.fetch_add(1, Ordering::SeqCst);
            if count < 2 {
                Err("Fail".to_string())
            } else {
                Ok("Success".to_string())
            }
        });

        match rx.recv_timeout(Duration::from_secs(2)) {
            Ok(Ok(val)) => assert_eq!(val, "Success"),
            _ => panic!("Expected success"),
        }
        assert_eq!(attempts.load(Ordering::SeqCst), 3);
    }

    #[test]
    fn test_concurrency_limit() {
        let q = Arc::new(TaskQueue::<()>::new(TaskOptions { concurrency: 2, rate_limit: None, max_retries: 0 }));
        let active = Arc::new(AtomicUsize::new(0));
        let max_active = Arc::new(AtomicUsize::new(0));

        let mut rxs = Vec::new();

        for _ in 0..10 {
            let active = active.clone();
            let max_active = max_active.clone();
            let rx = q.add(move || {
                let curr = active.fetch_add(1, Ordering::SeqCst) + 1;
                let mut max = max_active.load(Ordering::SeqCst);
                while curr > max {
                     let _ = max_active.compare_exchange(max, curr, Ordering::SeqCst, Ordering::SeqCst);
                     max = max_active.load(Ordering::SeqCst);
                }
                
                thread::sleep(Duration::from_millis(50));
                active.fetch_sub(1, Ordering::SeqCst);
                Ok(())
            });
            rxs.push(rx);
        }

        for rx in rxs {
            let _ = rx.recv_timeout(Duration::from_secs(5));
        }

        let peak = max_active.load(Ordering::SeqCst);
        // Peak should be <= 2 ideally.
        // But atomic check inside thread is racy vs the limit enforcer?
        // If enforcer is correct, we should never see > 2 active threads at once.
        assert!(peak <= 2, "Concurrency exceeded: {}", peak);
    }
}
