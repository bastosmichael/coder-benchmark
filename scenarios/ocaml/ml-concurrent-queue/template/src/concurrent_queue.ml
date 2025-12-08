module TaskQueue : sig
  type t
  val create : int -> (int * float) option -> int -> t
  val add : t -> (unit -> 'a) -> 'a
end = struct
  type t = unit
  let create _ _ _ = ()
  let add _ _ = failwith "Not implemented"
end
