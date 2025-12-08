<?php

require_once __DIR__ . '/../src/TaskQueue.php';

class TaskQueueTest {
    public function testRetries() {
        $queue = new TaskQueue(1, [], 2); // 2 retries
        $serverState = ['attempts' => 0];
        
        $queue->add(function() use (&$serverState) {
            $serverState['attempts']++;
            if ($serverState['attempts'] <= 2) {
                throw new Exception("Fail");
            }
            return "Success";
        });
        
        $results = $queue->run();
        
        assert(count($results) === 1);
        assert($results[0] === "Success");
        assert($serverState['attempts'] === 3); // 1 initial + 2 retries
        echo "Test Retries: PASSED\n";
    }

    public function testRateLimit() {
        // 2 tokens per 1 second
        $start = microtime(true);
        $queue = new TaskQueue(1, ['tokens' => 2, 'interval' => 1], 0);
        
        for ($i = 0; $i < 3; $i++) {
            $queue->add(function() { return "ok"; });
        }
        
        $queue->run();
        $duration = microtime(true) - $start;
        
        // Should take at least 1 second because 3rd task needs to wait for refresh
        // 0s: Task 1 (ok, 1 token left)
        // 0s: Task 2 (ok, 0 tokens left)
        // 0s: Task 3 (wait 1s, refill -> ok)
        // Total time approx 1s.
        
        assert($duration >= 0.9, "Duration should be at least ~1s, got $duration");
        echo "Test RateLimit: PASSED\n";
    }
}

try {
    $test = new TaskQueueTest();
    $test->testRetries();
    $test->testRateLimit();
    echo "All tests passed.\n";
} catch (Throwable $e) {
    echo "FAILED: " . $e->getMessage() . "\n";
    exit(1);
}
