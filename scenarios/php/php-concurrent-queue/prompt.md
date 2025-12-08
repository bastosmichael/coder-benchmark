You are a Backend Engineer.

Implement a `TaskQueue` class in PHP that manages the execution of closures with rate limiting and retry logic.

Class Structure:
```php
class TaskQueue {
    /**
     * @param int $concurrency Max parallel tasks (simulated or real).
     * @param array $rateLimit ['tokens' => int, 'interval' => int] 
     * @param int $maxRetries Max retries on exception.
     */
    public function __construct(int $concurrency, array $rateLimit = [], int $maxRetries = 0);

    /**
     * Add a closure task.
     * @param callable $task
     * @return void
     */
    public function add(callable $task);

    /**
     * Process all tasks.
     * Use usleep to simulate time passage if needed for rate limiting.
     * @return array Results of tasks
     */
    public function run(): array;
}
```

Requirements:
1. **Retries**: If a task throws, retry `maxRetries` times with exponential backoff (simulated sleep).
2. **Rate Limiting**: Implement a Token Bucket. Allow `tokens` per `interval` seconds.
3. **Execution**: Execute tasks sequentially (since PHP is single threaded) but respect the logic of rate limits and retries.
