You are a Scala Concurrency Expert.

Implement a `TaskQueue` class using standard `scala.concurrent.Future` and `Promise` or standard concurrency primitives.

Class Structure:
```scala
import scala.concurrent.{Future, Promise, ExecutionContext}
import scala.concurrent.duration._

class TaskQueue(concurrency: Int, rateLimit: Option[Map[String, Int]] = None, maxRetries: Int = 0)(implicit ec: ExecutionContext) {
  /**
    * Add a task to queue and return Future result.
    */
  def add[T](task: () => Future[T]): Future[T] = {
    // ...
  }
}
```

Requirements:
1. **Concurrency**: Ensure only `concurrency` tasks run in parallel.
2. **Rate Limiting**: Token bucket (tokens per interval).
3. **Retries**: Exponential backoff on failure.
4. **Non-blocking**: Use `Future` composition, do not block threads with `Await`.
