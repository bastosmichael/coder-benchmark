require_relative '../src/concurrent_queue'
require 'test/unit'

class TestTaskQueue < Test::Unit::TestCase
  def test_retries
    queue = TaskQueue.new(1, {}, 2)
    attempts = 0
    
    queue.add do
      attempts += 1
      raise "Fail" if attempts <= 2
      "Success"
    end
    
    results = queue.run
    
    assert_equal 1, results.length
    assert_equal "Success", results.first
    assert_equal 3, attempts
  end

  def test_concurrency
    # Check that we actually run in parallel
    queue = TaskQueue.new(3, {}, 0)
    
    3.times do
      queue.add do
         sleep 0.1
         Thread.current.object_id
      end
    end
    
    start = Time.now
    results = queue.run
    duration = Time.now - start
    
    # Should take roughly 0.1s if parallel, 0.3s if serial
    assert duration < 0.25, "Should run in parallel, took #{duration}"
    assert_equal 3, results.uniq.length # Should return 3 thread IDs (maybe same if pooled, but we want unique results?)
    # Actually results are return values. Thread IDs might reuse if pool is implemented efficiently, 
    # but concurrent execution implies different threads at same time.
    # The result array check is just to ensure we got 3 items.
  end
end
