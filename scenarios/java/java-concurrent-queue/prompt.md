
You are a Principal Software Engineer.

I have provided a stub implementation in `src/main/java/TaskQueue.java`.
Your task is to implement a robust, generic `TaskQueue` that handles concurrency management, rate limiting, and retries.

```java
package main.java;

import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Callable;

public class TaskQueue<T> {

    public static class RateLimit {
        public int tokens;
        public Duration interval;
        public RateLimit(int t, Duration i) { tokens = t; interval = i; }
    }

    public static class Options {
        public int concurrency;
        public RateLimit rateLimit;
        public int maxRetries;
        
        public Options(int c, RateLimit r, int m) {
            concurrency = c; rateLimit = r; maxRetries = m;
        }
    }

    public TaskQueue(Options options) {
    }

    /**
     * Adds a task to the queue.
     * @param task Callable to execute
     * @return Future resolving to result
     */
    public CompletableFuture<T> add(Callable<T> task) {
        // Implementation
        return new CompletableFuture<>(); // Stub
    }
}
```

Requirements:
1. **Concurrency Control**: At most `options.concurrency` tasks should run simultaneously (e.g. using a Thread Pool or Semaphore).
2. **Rate Limiting**: If `rateLimit` is provided, ensure tasks are started respecting the `tokens` per `interval` limit.
3. **Retries**: If a task throws an exception, it should be retried up to `maxRetries` times.
4. **Ordering**: Tasks should generally run in First-In-First-Out (FIFO) order.
5. **No External Libraries**: Use only standard Java (`java.util.concurrent`).

Correctness is critical. Deadlocks or race conditions are failures.
Output the full content of `src/main/java/TaskQueue.java` in a single markdown code block.
