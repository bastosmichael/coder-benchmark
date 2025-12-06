
You are a Go Expert.

Implement a generic `LRUCache[K comparable, V any]` in `src/lru.go`.

```go
package lru

type LRUCache[K comparable, V any] struct {
    // fields
}

func New[K comparable, V any](capacity int) *LRUCache[K, V] {
    return nil
}

func (c *LRUCache[K, V]) Get(key K) (V, bool) {
    var zero V
    return zero, false
}

func (c *LRUCache[K, V]) Put(key K, value V) {
    
}
```

Requirements:
1.  **O(1)** time.
2.  Use `container/list` and `map`.
3.  **Strict**: Go formatting.

Output full content of `src/lru.go`.
