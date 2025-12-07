
You are a Principal Software Engineer.

I have provided a stub implementation in `src/queue.go`.
Your task is to implement a robust, generic `TaskQueue` that handles concurrency management, rate limiting, and retries.

```go
package src

import (
	"context"
	"time"
)

type Task func(ctx context.Context) (interface{}, error)

type Options struct {
	Concurrency int
	RateLimit   *RateLimit // Optional
	MaxRetries  int
}

type RateLimit struct {
	Tokens   int
	Interval time.Duration
}

type TaskResult struct {
	Result interface{}
	Error  error
}

type TaskQueue struct {
    // fields
}

// NewTaskQueue creates a new queue instance
func NewTaskQueue(opts Options) *TaskQueue {
	return &TaskQueue{}
}

// Add submits a task and returns a channel that receives the result.
func (q *TaskQueue) Add(task Task) <-chan TaskResult {
    // implementation
	return nil
}

// Status returns stats
func (q *TaskQueue) Status() map[string]int {
	return map[string]int{"pending": 0, "running": 0, "completed": 0, "failed": 0}
}
```

Requirements:
1. **Concurrency Control**: At most `opts.Concurrency` tasks should run simultaneously as goroutines.
2. **Rate Limiting**: If `RateLimit` is provided, ensure tasks are started respecting the `Tokens` per `Interval` limit.
3. **Retries**: If a task fails (returns error), it should be retried up to `MaxRetries` times with exponential backoff.
4. **Ordering**: Tasks should generally run in First-In-First-Out (FIFO) order, respecting concurrency limits.
5. **No External Libraries**: Use only native Go features (goroutines, channels, sync, context).
6. **Thread Safety**: The queue must be safe for concurrent use.

Correctness is critical. Deadlocks or race conditions are failures.
Output the full content of `src/queue.go` in a single markdown code block.
