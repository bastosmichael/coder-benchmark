
package lru

import "testing"

func TestLRU(t *testing.T) {
    cache := New[int, int](2)
    cache.Put(1, 1)
    cache.Put(2, 2)
    
    if _, ok := cache.Get(1); !ok {
        t.Errorf("Get 1 failed")
    }
    
    cache.Put(3, 3) // Evicts 2
    
    if _, ok := cache.Get(2); ok {
        t.Errorf("Get 2 should be evicted")
    }
    
    if val, ok := cache.Get(3); !ok || val != 3 {
        t.Errorf("Get 3 failed")
    }
}
