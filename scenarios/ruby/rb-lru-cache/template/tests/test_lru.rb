
require_relative '../src/lru_cache'
require 'test/unit'

class TestLRU < Test::Unit::TestCase
  def test_basic
    cache = LRUCache.new(2)
    cache.put(1, 1)
    cache.put(2, 2)
    
    assert_equal(1, cache.get(1))
    
    cache.put(3, 3) # evicts 2
    
    assert_nil(cache.get(2))
    assert_equal(3, cache.get(3))
    assert_equal(1, cache.get(1))
  end
end
