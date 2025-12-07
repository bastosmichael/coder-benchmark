#pragma once
#include <functional>
#include <future>
#include <mutex>
#include <queue>
#include <condition_variable>
#include <thread>
#include <vector>
#include <chrono>
#include <optional>
#include <stdexcept>

struct RateLimit {
    int tokens;
    std::chrono::milliseconds interval;
};

struct Options {
    int concurrency;
    std::optional<RateLimit> rateLimit;
    int maxRetries;
};

class TaskQueue {
public:
    TaskQueue(Options opts) {}
    ~TaskQueue() {}

    template<typename F, typename R = std::invoke_result_t<F>>
    std::future<R> add(F&& f) {
        throw std::runtime_error("Not implemented");
    }
};
