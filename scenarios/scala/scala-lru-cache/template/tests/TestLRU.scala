
object TestLRU {
  def assertEq[T](actual: T, expected: T, msg: String): Unit = {
    if (actual != expected) {
      println(s"FAILURE: $msg. Expected $expected, got $actual")
      System.exit(1)
    }
  }

  def main(args: Array[String]): Unit = {
    val cache = new LRUCache[Int, Int](2)
    cache.put(1, 1)
    cache.put(2, 2)
    
    assertEq(cache.get(1), Some(1), "Get 1")
    
    cache.put(3, 3) // Evicts 2, because 1 was just accessed? 
    // Wait, in my test above I accessed 1 -> cache.get(1). So 1 is MRU. 2 is LRU.
    // So 2 should be evicted.
    
    assertEq(cache.get(2), None, "Get 2 should be None (evicted)")
    assertEq(cache.get(3), Some(3), "Get 3")
    
    // 1 should still be there
    assertEq(cache.get(1), Some(1), "Get 1 should still exist")
    
    println("All tests passed")
  }
}
