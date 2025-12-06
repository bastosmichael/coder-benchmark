
using Xunit;
using LRU;

public class TestLRU {
    [Fact]
    public void TestBasic() {
        var cache = new LRUCache<int, int>(2);
        cache.Put(1, 1);
        cache.Put(2, 2);
        
        Assert.True(cache.TryGet(1, out var v1));
        Assert.Equal(1, v1);
        
        cache.Put(3, 3); // Evicts 2
        
        Assert.False(cache.TryGet(2, out _));
        Assert.True(cache.TryGet(3, out var v3));
        Assert.Equal(3, v3);
    }
}
