class TaskQueue
  def initialize(concurrency, rate_limit = {}, max_retries = 0)
    # ...
  end

  def add(&block)
    # ...
  end

  def run
    []
  end
end
