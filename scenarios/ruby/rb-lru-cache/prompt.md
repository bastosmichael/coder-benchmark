
You are a Ruby Expert.

Implement `LRUCache` in `src/lru_cache.rb`.

```ruby
class LRUCache
  def initialize(capacity)
  end

  def get(key)
    # Return nil if not found
  end

  def put(key, value)
  end
end
```

Requirements:
1.  **O(1)**.
2.  Can use `Hash` (Ruby hashes are ordered by insertion since 1.9).
    *   **Hint**: Just deleting and re-inserting keeps the order for MRU.
3.  **Strict**: Valid Ruby syntax.

Output full content of `src/lru_cache.rb`.
