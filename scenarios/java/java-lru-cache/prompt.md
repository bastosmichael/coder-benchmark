
You are a Java Expert.

Implement a generic `LRUCache<K, V>` in `src/LRUCache.java`.

```java
import java.util.*;

public class LRUCache<K, V> {
    // Add fields. LinkedHashMap is allowed but for "Senior" level, 
    // maybe implement the node handling manually or use LinkedHashMap's removeEldestEntry?
    // Let's ask to Use LinkedHashMap for simplicity but ensure O(1).

    public LRUCache(int capacity) {
        
    }

    public V get(K key) {
        // Return null if not found
        return null;
    }

    public void put(K key, V value) {
        
    }
}
```

Requirements:
1.  **O(1)** time complexity.
2.  **Thread safety NOT required**.
3.  **Strict**: Must verify with `javac`.

Output full content of `src/LRUCache.java`.
