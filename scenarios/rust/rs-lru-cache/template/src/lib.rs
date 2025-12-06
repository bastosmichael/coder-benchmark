
pub struct LRUCache<K, V> {
    cap: usize,
    _phantom: std::marker::PhantomData<(K, V)>,
}

impl<K, V> LRUCache<K, V> {
    pub fn new(capacity: usize) -> Self {
        Self { cap: capacity, _phantom: std::marker::PhantomData }
    }
}
