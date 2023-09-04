module Poly : sig
  type 'a t

  val create : ?fp_prob:float -> int -> 'a t
  val add : 'a t -> 'a -> unit
  val is_present : 'a t -> 'a -> bool
  val clear : 'a t -> unit
  val length : 'a t -> int
  val accumulate : 'a t -> 'a t -> 'a t
  val contains : 'a t -> 'a t -> bool
  val copy : 'a t -> 'a t
  val to_bytes : 'a t -> bytes
  val from_bytes : bytes -> 'a t
end

include module type of Poly

module type Seeded_Hashable = sig
  type t

  val hash : int -> t -> int
end

module type S = sig
  type value
  type t

  val add : t -> value -> unit
  val is_present : t -> value -> bool
  val create : ?fp_prob:float -> int -> t
  val clear : t -> unit
  val length : t -> int
  val accumulate : t -> t -> t
  val contains : t -> t -> bool
  val copy : t -> t
  val to_bytes : t -> bytes
  val from_bytes : bytes -> t
end

module MakeSeeded (H : Seeded_Hashable) : S with type value = H.t
