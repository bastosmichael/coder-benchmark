open Concurrent_queue
open Printf

let test_retries () =
  let q = TaskQueue.create 1 None 2 in
  let attempts = ref 0 in
  
  try
    let res = TaskQueue.add q (fun () ->
      incr attempts;
      if !attempts <= 2 then failwith "Fail" else "Success"
    ) in
    if res <> "Success" then failwith "Result mismatch";
    if !attempts <> 3 then failwith "Retry count mismatch";
    printf "Test Retries: PASSED\n"
  with_ -> failwith "Exception raised"

let test_concurrency () =
  let q = TaskQueue.create 3 None 0 in
  let finished = ref 0 in
  let m = Mutex.create () in
  
  let task () =
    Thread.delay 0.1;
    Mutex.lock m;
    incr finished;
    Mutex.unlock m;
    "Done"
  in
  
  let t1 = Thread.create (fun () -> TaskQueue.add q task) () in
  let t2 = Thread.create (fun () -> TaskQueue.add q task) () in
  let t3 = Thread.create (fun () -> TaskQueue.add q task) () in
  
  Thread.join t1;
  Thread.join t2;
  Thread.join t3;
  
  if !finished <> 3 then failwith "Tasks didn't finish";
  printf "Test Concurrency: PASSED\n"

let () =
  try
    test_retries ();
    test_concurrency ();
    printf "All tests passed.\n"
  with Failure f ->
    printf "FAILED: %s\n" f;
    exit 1
  | e -> 
    printf "FAILED: Exception\n";
    exit 1
