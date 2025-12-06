
open Lru_cache

module IntKey = struct
  type t = int
  let compare = compare
end

module Cache = Make(IntKey)

let assert_eq a b msg =
  if a <> b then (
    print_endline ("Assertion failed: " ^ msg);
    exit 1
  )

let () =
  let c = Cache.create 2 in
  
  Cache.put c 1 1;
  Cache.put c 2 2;
  
  match Cache.get c 1 with
  | Some v -> assert_eq v 1 "Get 1"
  | None -> assert_eq 0 1 "Get 1 failed"; (* Fail *)
  ;
  
  Cache.put c 3 3; (* Evicts 2 *)
  
  match Cache.get c 2 with
  | Some _ -> assert_eq 0 1 "Get 2 should be evicted"
  | None -> ()
  ;
  
  match Cache.get c 3 with
  | Some v -> assert_eq v 3 "Get 3"
  | None -> assert_eq 0 1 "Get 3 failed"
  ;
  
  print_endline "All tests passed"
