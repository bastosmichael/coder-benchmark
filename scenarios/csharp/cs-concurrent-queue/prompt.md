
You are a Principal Software Engineer.

I have provided a stub implementation in `src/TaskQueue.cs`.
Your task is to implement a robust, generic `TaskQueue` that handles concurrency management, rate limiting, and retries.

```csharp
using System;
using System.Threading;
using System.Threading.Tasks;

namespace ConcurrentQueue;

public class TaskOptions
{
    public int Concurrency { get; set; }
    public RateLimit? RateLimit { get; set; }
    public int MaxRetries { get; set; }
}

public class RateLimit
{
    public int Tokens { get; set; }
    public TimeSpan Interval { get; set; }
}

public class TaskQueue<T>
{
    // .. fields

    public TaskQueue(TaskOptions options)
    {
    }

    /// <summary>
    /// Adds a task to the queue and returns a Task that completes with the result.
    /// </summary>
    public Task<T> Add(Func<CancellationToken, Task<T>> taskGenerator)
    {
        // implementation
        throw new NotImplementedException();
    }
}
```

Requirements:
1. **Concurrency Control**: At most `options.Concurrency` tasks should run simultaneously in parallel. (e.g. `SemaphoreSlim`).
2. **Rate Limiting**: If `RateLimit` is provided, ensure tasks start respecting `Tokens` per `Interval`. (Token Bucket or Leaky Bucket).
3. **Retries**: If a task throws an exception, retry up to `MaxRetries` times.
4. **Ordering**: Tasks should generally be processed in FIFO order.
5. **No External Libraries**: Use only `System.Threading` / `System.Threading.Tasks`.

Correctness is critical.
Output the full content of `src/TaskQueue.cs` in a single markdown code block.
