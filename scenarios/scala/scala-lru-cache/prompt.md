
You are a Scala Expert.

Implement a generic `LRUCache` in `src/LRUCache.scala`.

```scala
import scala.collection.mutable

class LRUCache[K, V](capacity: Int) {
  // Add fields here. simple mutable.LinkedHashMap is often used for O(1).
  
  def get(key: K): Option[V] = {
    ???
  }

  def put(key: K, value: V): Unit = {
    ???
  }
}
```

Requirements:
1.  **O(1)** time complexity for `get` and `put`. 
2.  Use standard Scala collections (e.g., `mutable.LinkedHashMap` or implement your own doubly linked list).
3.  **Thread safety is NOT required** for this benchmark (simplicity).
4.  **Strict**: Must compile without warnings.

Output full content of `src/LRUCache.scala`.
