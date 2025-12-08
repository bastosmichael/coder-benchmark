<?php

class TaskQueue {
    public function __construct(int $concurrency, array $rateLimit = [], int $maxRetries = 0) {
        // ...
    }

    public function add(callable $task) {
        // ...
    }

    public function run(): array {
        return [];
    }
}
