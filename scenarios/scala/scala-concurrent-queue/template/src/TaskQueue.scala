import scala.concurrent.{Future, ExecutionContext}

class TaskQueue(concurrency: Int, rateLimit: Option[Map[String, Int]] = None, maxRetries: Int = 0)(implicit ec: ExecutionContext) {
  def add[T](task: () => Future[T]): Future[T] = {
    Future.failed(new Exception("Not implemented"))
  }
}
