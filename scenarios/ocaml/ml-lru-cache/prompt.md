
You are an OCaml Expert.

Implement a generic `LRUCache` in `src/lru_cache.ml` using a functor or a polymorphic class.
For simplicity in this benchmark, implement a `Make` functor that takes a comparable Key module.

```ocaml
module type KeySig = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type k
  type 'a t
  
  (* Create a cache with capacity *)
  val create : int -> 'a t
  
  (* Get value *)
  val get : 'a t -> k -> 'a option
  
  (* Put value *)
  val put : 'a t -> k -> 'a -> unit
end

module Make (Key : KeySig) : S with type k = Key.t
```

Requirements:
1.  **Mutability**: You may use `Hashtbl` and mutable fields for the doubly linked list.
2.  **O(1)** operations.
3.  **Strict**: OCaml type system must be respected.

Output full content of `src/lru_cache.ml`.
