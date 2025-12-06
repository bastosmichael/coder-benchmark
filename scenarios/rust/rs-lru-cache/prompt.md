
You are a Rust Expert.

Implement a generic LRU Cache in Rust in `src/lib.rs`.

```rust
use std::hash::Hash;
use std::collections::HashMap;

pub struct LRUCache<K, V> {
    // Add fields here
}

impl<K: Hash + Eq + Clone, V> LRUCache<K, V> {
    pub fn new(capacity: usize) -> Self {
        unimplemented!()
    }

    pub fn get(&mut self, key: &K) -> Option<&V> {
        unimplemented!()
    }

    pub fn put(&mut self, key: K, value: V) {
        unimplemented!()
    }
}
```

Requirements:
1.  **O(1)** time complexity for `get` and `put` (amortized).
2.  Use safe Rust preferably, but interior mutability or `Rc<RefCell<>>` might be needed for a doubly linked list approach if standard `LinkedList` is awkward (cursor methods are experimental). 
3.  Actually, using `std::collection::HashMap` + a tracker Vector or List is tricky for O(1).
4.  **Simpler Requirement for LLMs**: We accept O(N) scan for `put` eviction if O(1) is too hard in safe Rust without crates like `lru`. 
    *   *Wait*, the prompt asks for "Senior Software Engineer" level. 
    *   Reference: Python/TS use `Map`/`OrderedDict` which preserves order. Rust `HashMap` does not. 
    *   **Challenge**: Implement it correctly. If they use a `Vec` for order, it's O(N). If they use `linked-hash-map` logic from scratch, it's unsafe or complex.
    *   **Let's relax** the O(1) constraint to "efficiently as possible in Safe Rust" or explicitly allow `unsafe` if needed, OR imply they can use `Vec` of keys for MRU tracking (O(N) eviction) but give full points only if they try better? 
    *   No, let's keep it standard. `get` must be O(1). `put` eviction can be O(N) if they are lazy, but we prefer O(1).
    *   Actually, let's just ask for functionality first. Building Rust code that **compiles** with lifetimes is the hard part.

Output full content of `src/lib.rs`.
