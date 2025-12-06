
using System;

namespace LRU {
    public class LRUCache<K, V> {
        public LRUCache(int capacity) {}
        public bool TryGet(K key, out V value) {
            value = default!;
            return false;
        }
        public void Put(K key, V value) {}
    }
}
