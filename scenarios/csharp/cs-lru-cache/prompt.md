
You are a C# Expert.

Implement a generic `LRUCache<K, V>` in `src/LRUCache.cs`.

```csharp
using System;
using System.Collections.Generic;

namespace LRU {
    public class LRUCache<K, V> {
        // Fields

        public LRUCache(int capacity) {
        }

        public V Get(K key) {
            // Return default(V) or throw if not found? 
            // Let's use TryGet pattern or return default check. 
            // Standard C# `Dictionary` uses throws or TryGetValue.
            // Let's implement TryGetValue or just Get returning default(V) for simplicity if V is ref type?
            // Let's simplify: Return tuple or use `bool TryGet(K key, out V value)`.
            throw new NotImplementedException();
        }

        public void Put(K key, V value) {
        }
    }
}
```

Requirements:
1.  **O(1)** time.
2.  Use `Dictionary` + `LinkedList` or similar.
3.  **Strict**: Clean code.

Output full content of `src/LRUCache.cs`.
