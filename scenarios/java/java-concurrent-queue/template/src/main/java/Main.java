package main.java;
import java.util.concurrent.*;
import java.time.Duration;
import java.util.concurrent.atomic.*;
import java.util.*;

public class Main {
    static void testBasic() throws Exception {
        TaskQueue.Options opts = new TaskQueue.Options(1, null, 0);
        TaskQueue<String> q = new TaskQueue<>(opts);
        CompletableFuture<String> f = q.add(() -> "ok");
        if(!"ok".equals(f.get(1, TimeUnit.SECONDS))) throw new RuntimeException("Wrong result");
    }

    static void testConcurrency() throws Exception {
        TaskQueue.Options opts = new TaskQueue.Options(2, null, 0);
        TaskQueue<Void> q = new TaskQueue<>(opts);
        
        AtomicInteger active = new AtomicInteger(0);
        AtomicInteger maxActive = new AtomicInteger(0);
        List<CompletableFuture<Void>> futures = new ArrayList<>();
        
        for(int i=0; i<10; i++) {
            futures.add(q.add(() -> {
                int curr = active.incrementAndGet();
                maxActive.accumulateAndGet(curr, Math::max);
                Thread.sleep(50);
                active.decrementAndGet();
                return null;
            }));
        }
        
        for(CompletableFuture<Void> f : futures) f.get(2, TimeUnit.SECONDS);
        
        if (maxActive.get() > 2) throw new RuntimeException("Concurrency exceeded: " + maxActive.get());
    }

    public static void main(String[] args) {
        try {
            testBasic();
            testConcurrency();
            System.out.println("Tests passed");
        } catch(Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }
}
