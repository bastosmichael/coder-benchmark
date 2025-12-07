package src

import (
	"context"
	"errors"
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
}

// NewTaskQueue creates a new queue instance
func NewTaskQueue(opts Options) *TaskQueue {
	return &TaskQueue{}
}

// Add submits a task and returns a channel that receives the result.
func (q *TaskQueue) Add(task Task) <-chan TaskResult {
	ch := make(chan TaskResult, 1)
	ch <- TaskResult{Error: errors.New("not implemented")}
	close(ch)
	return ch
}

// Status returns stats
func (q *TaskQueue) Status() map[string]int {
	return map[string]int{"pending": 0, "running": 0, "completed": 0, "failed": 0}
}
