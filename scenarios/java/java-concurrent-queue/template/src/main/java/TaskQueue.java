package main.java;

import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Callable;

public class TaskQueue<T> {

    public static class RateLimit {
        public int tokens;
        public Duration interval;
        public RateLimit(int t, Duration i) { tokens = t; interval = i; }
    }

    public static class Options {
        public int concurrency;
        public RateLimit rateLimit;
        public int maxRetries;
        
        public Options(int c, RateLimit r, int m) {
            concurrency = c; rateLimit = r; maxRetries = m;
        }
    }

    public TaskQueue(Options options) {
    }

    public CompletableFuture<T> add(Callable<T> task) {
        throw new RuntimeException("Not implemented");
    }
}
