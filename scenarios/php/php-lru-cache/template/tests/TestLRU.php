<?php
require_once __DIR__ . '/../src/LRUCache.php';

function assertEq($actual, $expected, $msg) {
    if ($actual !== $expected) {
        echo "FAILURE: $msg. Expected $expected, got $actual\n";
        exit(1);
    }
}

$cache = new LRUCache(2);
$cache->put(1, 1);
$cache->put(2, 2);

assertEq($cache->get(1), 1, "Get 1");

$cache->put(3, 3); // evicts 2

assertEq($cache->get(2), null, "Get 2 (evicted)");
assertEq($cache->get(3), 3, "Get 3");
assertEq($cache->get(1), 1, "Get 1 (preserved)");

echo "All tests passed\n";
