You are a OCaml Concurrency Expert.

Implement a `TaskQueue` module using standard `Thread`, `Mutex`, and `Condition`.

```ocaml
module TaskQueue : sig
  type t
  val create : int -> (int * float) option -> int -> t
  val add : t -> (unit -> 'a) -> 'a
end = struct
  (* ... *)
end
```

Requirements:
1. **Concurrency**: Limit parallel tasks to `n`.
2. **Rate Limiting**: Token bucket.
3. **Retries**: Exponential backoff.
4. **Blocking**: `add` blocks until task is executed and returns result.
