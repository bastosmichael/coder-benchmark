package src

import (
	"context"
	"errors"
	"sync"
	"sync/atomic"
	"testing"
	"time"
)

func TestBasicExecution(t *testing.T) {
	q := NewTaskQueue(Options{Concurrency: 2})
	ch := q.Add(func(ctx context.Context) (interface{}, error) {
		return "ok", nil
	})

	select {
	case res := <-ch:
		if res.Error != nil {
			t.Fatalf("Task failed: %v", res.Error)
		}
		if res.Result != "ok" {
			t.Errorf("Unexpected result: %v", res.Result)
		}
	case <-time.After(1 * time.Second):
		t.Fatal("Timeout")
	}
}

func TestConcurrency(t *testing.T) {
	concurrency := 3
	q := NewTaskQueue(Options{Concurrency: concurrency})

	var running int32
	var maxRunning int32

	count := 10
	var wg sync.WaitGroup
	wg.Add(count)

	for i := 0; i < count; i++ {
		q.Add(func(ctx context.Context) (interface{}, error) {
			defer wg.Done()
			curr := atomic.AddInt32(&running, 1)
			if curr > maxRunning {
				// Simple check, not strictly atomic max but good enough for test
				// To be precise:
				for {
					oldMax := atomic.LoadInt32(&maxRunning)
					if curr <= oldMax {
						break
					}
					if atomic.CompareAndSwapInt32(&maxRunning, oldMax, curr) {
						break
					}
				}
			}
			if int(curr) > concurrency {
				t.Errorf("Concurrency violation! Running: %d", curr)
			}
			time.Sleep(50 * time.Millisecond)
			atomic.AddInt32(&running, -1)
			return nil, nil
		})
	}

	wg.Wait()
	if int(maxRunning) > concurrency {
		t.Errorf("Max concurrency %d exceeded limit %d", maxRunning, concurrency)
	}
}

func TestRetries(t *testing.T) {
	q := NewTaskQueue(Options{Concurrency: 1, MaxRetries: 2})

	var attempts int32
	ch := q.Add(func(ctx context.Context) (interface{}, error) {
		val := atomic.AddInt32(&attempts, 1)
		if val <= 2 {
			return nil, errors.New("fail")
		}
		return "success", nil
	})

	res := <-ch
	if res.Error != nil {
		t.Fatalf("Expected success after retry, got error: %v", res.Error)
	}
	if attempts != 3 {
		t.Errorf("Expected 3 attempts, got %d", attempts)
	}
}

func TestRateLimit(t *testing.T) {
	// 5 tokens per 1 second
	q := NewTaskQueue(Options{
		Concurrency: 10,
		RateLimit: &RateLimit{
			Tokens:   5,
			Interval: 500 * time.Millisecond,
		},
	})

	start := time.Now()
	var wg sync.WaitGroup
	count := 10
	wg.Add(count)

	for i := 0; i < count; i++ {
		q.Add(func(ctx context.Context) (interface{}, error) {
			wg.Done()
			return nil, nil
		})
	}

	wg.Wait()
	duration := time.Since(start)

	// 10 tasks, 5 per 500ms.
	// Batch 1 (5) runs immediately (t=0).
	// Batch 2 (5) runs after 500ms (t=500ms).
	// Total time should be at least 500ms.

	if duration < 400*time.Millisecond {
		t.Errorf("Tasks ran too fast: %v", duration)
	}
}
