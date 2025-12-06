
package lru

type LRUCache[K comparable, V any] struct{}

func New[K comparable, V any](capacity int) *LRUCache[K, V] {
    return &LRUCache[K, V]{}
}

func (c *LRUCache[K, V]) Get(key K) (V, bool) {
    var zero V
    return zero, false
}

func (c *LRUCache[K, V]) Put(key K, value V) {}
