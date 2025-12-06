
module type KeySig = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type k
  type 'a t
  val create : int -> 'a t
  val get : 'a t -> k -> 'a option
  val put : 'a t -> k -> 'a -> unit
end

module Make (Key : KeySig) = struct
  type k = Key.t
  type 'a t = unit
  
  let create _ = ()
  let get _ _ = None
  let put _ _ _ = ()
end
