#include <iostream>
#include <thread>
#include <chrono>
#include <cassert>
#include <vector>
#include <atomic>
#include "queue.hpp"

void testBasic() {
    Options opts{1, std::nullopt, 0};
    TaskQueue q(opts);
    auto f = q.add([]{ return 42; });
    // This will throw if not implemented
    
    // In stub, add throws immediately. We catch strict not implemented.
    // If implemented, f.get() blocks.
    try {
        if(f.get() != 42) {
             std::cerr << "Wrong result\n";
             exit(1);
        }
    } catch(const std::exception& e) {
        // If it's "not implemented", fail test gracefully or hard?
        // Hard fail is fine.
        std::cerr << "Fail: " << e.what() << "\n";
        exit(1);
    }
}

void testConcurrency() {
    Options opts{2, std::nullopt, 0};
    TaskQueue q(opts);
    std::atomic<int> active{0};
    std::atomic<bool> failed{false};
    
    std::vector<std::future<void>> futures;
    for(int i=0; i<10; i++) {
        futures.push_back(q.add([&]{
            active++;
            if(active > 2) failed = true;
            std::this_thread::sleep_for(std::chrono::milliseconds(50));
            active--;
        }));
    }
    
    for(auto& f : futures) f.get();
    
    if(failed) {
        std::cerr << "Concurrency broken\n";
        exit(1);
    }
}

int main() {
    try {
        testBasic();
        testConcurrency();
        std::cout << "Tests passed\n";
    } catch(...) { 
        return 1; 
    }
    return 0;
}
