You are a Haskell Concurrency Expert.

Implement a `TaskQueue` module that manages concurrent task execution.

```haskell
module TaskQueue (TaskQueue, newQueue, addTask) where

import Control.Concurrent
import Control.Concurrent.MVar
-- import Control.Concurrent.STM -- Optional if available

data TaskQueue = TaskQueue {
    -- ...
}

-- | Create a new queue with max concurrency, rate limits (tokens/interval), and retries
newQueue :: Int -> Maybe (Int, Int) -> Int -> IO TaskQueue
newQueue concurrency rateLimit maxRetries = ...

-- | Add a task (IO action) and return an IO action that waits for the result
addTask :: TaskQueue -> IO a -> IO (IO a)
addTask q action = ...
```

Requirements:
1. **Concurrency**: Limit running tasks to `concurrency`.
2. **Rate Limiting**: Token bucket (tokens per microsecond interval).
3. **Retries**: Exponential backoff.
4. **Interface**: `addTask` returns an action that, when executed, blocks until the task is done and returns result.
