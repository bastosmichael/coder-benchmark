
#[cfg(test)]
mod tests {
    use super::*; // assumes the impl is in lib.rs logic, but we need to import `rs_lru_cache::LRUCache`? 
    // Usually integration tests are in tests/*.rs, unit tests in src/*.rs. 
    // This file is at 'tests/integration_test.rs' ? No, previous step instructions put strict directory structure.
    // Let's assume prompt allows `use crate::LRUCache`. 
    // But since `template/src/lib.rs` declares it, we can use `rs_lru_cache` crate name if defined in Cargo.toml.
    
    use rs_lru_cache::LRUCache;

    #[test]
    fn test_basic() {
        let mut cache = LRUCache::new(2);
        cache.put(1, 1);
        cache.put(2, 2);
        assert_eq!(cache.get(&1), Some(&1));
        
        cache.put(3, 3); // evicts 2
        assert_eq!(cache.get(&2), None);
        
        cache.put(4, 4); // evicts 1
        assert_eq!(cache.get(&1), None);
        assert_eq!(cache.get(&3), Some(&3));
    }
}
