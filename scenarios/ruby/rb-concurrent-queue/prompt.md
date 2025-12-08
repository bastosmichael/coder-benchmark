You are a Ruby Concurrency Expert.

Implement a `TaskQueue` class that safely handles concurrent task execution with rate limiting and retries.

Class Structure:
```ruby
class TaskQueue
  # @param concurrency [Integer] Max parallel threads
  # @param rate_limit [Hash] { tokens: Integer, interval: Float }
  # @param max_retries [Integer]
  def initialize(concurrency, rate_limit = {}, max_retries = 0)
    # ...
  end

  # @param block [Proc] Task to execute
  def add(&block)
    # Adds task to queue
  end

  # Blocks until all tasks are complete, returns results
  def run
    # ...
  end
end
```

Requirements:
1. **Thread Safety**: Use `Monitor`, `Mutex`, or `Queue`.
2. **Rate Limiting**: Token Bucket.
3. **Retries**: Exponential backoff.
4. **Concurrency**: Execute tasks in a thread pool of size `concurrency`.
