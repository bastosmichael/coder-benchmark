
public class TestLRU {
    public static void main(String[] args) {
        LRUCache<Integer, Integer> cache = new LRUCache<>(2);
        
        cache.put(1, 1);
        cache.put(2, 2);
        
        assertEq(cache.get(1), 1, "Get 1");
        
        cache.put(3, 3); // Evicts 2
        
        assertEq(cache.get(2), null, "Get 2 (evicted)");
        assertEq(cache.get(3), 3, "Get 3");
        assertEq(cache.get(1), 1, "Get 1 (preserved)");
        
        System.out.println("All tests passed");
    }
    
    private static <T> void assertEq(T actual, T expected, String msg) {
        boolean match = (actual == null && expected == null) || (actual != null && actual.equals(expected));
        if (!match) {
            System.err.println("FAILURE: " + msg + ". Expected " + expected + ", got " + actual);
            System.exit(1);
        }
    }
}
