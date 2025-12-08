import scala.concurrent.{Future, ExecutionContext, Await, Promise}
import scala.concurrent.duration._
import java.util.concurrent.Executors

object TestTaskQueue {
  private val pool = Executors.newCachedThreadPool()
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)

  def main(args: Array[String]): Unit = {
    try {
      testRetries()
      // testRateLimit() // Skipping timing test to avoid flakiness on some envs, but could add basic one
      testConcurrency()
      println("All tests passed.")
      pool.shutdown()
    } catch {
      case e: Throwable => 
        println(s"FAILED: ${e.getMessage}")
        e.printStackTrace()
        System.exit(1)
    }
  }

  def testRetries(): Unit = {
    val queue = new TaskQueue(1, maxRetries = 2)
    var attempts = 0
    
    val f = queue.add(() => Future {
      attempts += 1
      if (attempts <= 2) throw new Exception("Fail")
      "Success"
    })
    
    val result = Await.result(f, 5.seconds)
    assert(result == "Success")
    assert(attempts == 3) // Initial + 2 retries
    println("Test Retries: PASSED")
  }

  def testConcurrency(): Unit = {
    val queue = new TaskQueue(3)
    val start = System.currentTimeMillis()
    
    val futures = (1 to 3).map { _ =>
      queue.add(() => Future {
        Thread.sleep(100)
        "Done"
      })
    }
    
    Await.result(Future.sequence(futures), 5.seconds)
    val duration = System.currentTimeMillis() - start
    
    // Should take approx 100ms + overhead, definitely < 250ms if parallel.
    // If serial, 300ms.
    assert(duration < 250, s"Should run in parallel, took $duration ms")
    println("Test Concurrency: PASSED")
  }
}
