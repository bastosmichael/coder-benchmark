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
    public TaskQueue(TaskOptions options)
    {
    }

    public Task<T> Add(Func<CancellationToken, Task<T>> taskGenerator)
    {
        throw new NotImplementedException();
    }
}
