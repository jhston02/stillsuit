type 'a t

val add : 'a t -> 'a -> unit

val is_present: 'a t -> 'a -> bool

val create : ?fp_prob:float -> int -> 'a t

val clear: 'a t -> unit

val length: 'a t -> int

val accumulate: 'a t -> 'a t -> 'a t

val contains: 'a t -> 'a t -> bool

val copy: 'a t -> 'a t

val to_bytes: 'a t -> bytes

val from_bytes: bytes -> 'a t


module type Seeded_Hashable = sig
  type t
  val hash: int -> t -> int
end

module type S = sig
  type value
  type 'a t
  val add: 'a t -> value -> unit
  val is_present: 'a t -> value -> bool
  val create : int -> fp_prob:float -> 'a t
  val clear: 'a t -> unit
  val length: 'a t -> int
  val accumulate: 'a t -> 'a t -> 'a t
  val contains: 'a t -> 'a t -> bool
  val copy: 'a t -> 'a t
  val to_bytes: 'a t -> bytes
  val from_bytes: bytes -> 'a t
end


module MakeSeeded (H : Seeded_Hashable) : S with type value = H.t