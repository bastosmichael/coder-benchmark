using System;
using System.Threading;
using System.Threading.Tasks;
using System.Diagnostics;
using ConcurrentQueue;

class Program
{
    static async Task TestBasic()
    {
        var opts = new TaskOptions { Concurrency = 1, MaxRetries = 0 };
        var q = new TaskQueue<string>(opts);
        var res = await q.Add(async (ct) => {
            await Task.Yield();
            return "ok";
        });
        if (res != "ok") throw new Exception("Unexpected result");
    }

    static async Task TestConcurrency()
    {
        var opts = new TaskOptions { Concurrency = 2 };
        var q = new TaskQueue<int>(opts);
        int running = 0;
        int maxRunning = 0;
        object lockObj = new object();

        var tasks = new List<Task>();
        for(int i=0; i<10; i++)
        {
            tasks.Add(q.Add(async (ct) => {
                lock(lockObj) {
                    running++;
                    maxRunning = Math.Max(maxRunning, running);
                }
                
                if (running > 2) throw new Exception("Concurrency Violation");
                
                await Task.Delay(50);
                
                lock(lockObj) {
                    running--;
                }
                return 0;
            }));
        }
        await Task.WhenAll(tasks);
        
        if (maxRunning == 0) throw new Exception("Did not run");
    }

    static async Task Main(string[] args)
    {
        try
        {
            await TestBasic();
            await TestConcurrency();
            Console.WriteLine("Tests passed");
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Test failed: {ex}");
            Environment.Exit(1);
        }
    }
}
