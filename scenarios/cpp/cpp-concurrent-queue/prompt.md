
You are a Principal Software Engineer.

I have provided a stub implementation in `src/queue.hpp`.
Your task is to implement a robust, generic `TaskQueue` that handles concurrency management, rate limiting, and retries.

```cpp
#pragma once
#include <functional>
#include <future>
#include <mutex>
#include <queue>
#include <condition_variable>
#include <thread>
#include <vector>
#include <chrono>

struct RateLimit {
    int tokens;
    std::chrono::milliseconds interval;
};

struct Options {
    int concurrency;
    std::optional<RateLimit> rateLimit;
    int maxRetries;
};

class TaskQueue {
public:
    TaskQueue(Options opts);
    ~TaskQueue();

    // Adds a task to the queue. 
    // Returns a future that resolves when the task completes.
    // Task is a simple void function for simplicity, but you should handle exceptions inside and retries.
    // To support returning values, you can use std::packaged_task internally.
    // For this stub, we assume generic F returning R.
    template<typename F, typename R = std::invoke_result_t<F>>
    std::future<R> add(F&& f);
    
    // ... Implement logic
};
```
(Note: You can implement the Template method in the header).

Requirements:
1. **Concurrency Control**: At most `opts.concurrency` tasks should run simultaneously in threads (Thread Pool pattern).
2. **Rate Limiting**: If `rateLimit` is provided, ensure tasks are started respecting the `tokens` per `interval` limit.
3. **Retries**: If a task throws an exception, it should be retried up to `maxRetries` times.
4. **Ordering**: Tasks should generally run in First-In-First-Out (FIFO) order.
5. **No External Libraries**: Use only C++ Standard Library (`std::thread`, `std::mutex`, etc.).

Correctness is critical. Deadlocks or race conditions are failures.
Output the full content of `src/queue.hpp` in a single markdown code block.
