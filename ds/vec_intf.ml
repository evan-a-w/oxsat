open! Core

module type Elt = sig
  type t

  val create_for_vec : unit -> t
end

module type S = sig
  module Elt : Elt

  type t

  val create : ?capacity:int -> unit -> t
  val length : t -> int
  val get : t -> int -> Elt.t
  val set : t -> int -> Elt.t -> unit
  val iter_rev : t -> f:(Elt.t -> unit) -> unit
  val last_exn : t -> Elt.t
  val push : t -> Elt.t -> unit
  val pop_exn : t -> Elt.t
end

module type S_value = sig
  type 'a t

  val create : ?capacity:int -> unit -> 'a t
  val length : 'a t -> int
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
  val iter_rev : 'a t -> f:('a -> unit) -> unit
  val push : 'a t -> 'a -> unit
  val pop_exn : 'a t -> 'a
  val fill_to_length : 'a t -> length:int -> f:(int -> 'a) -> unit
  val clear : 'a t -> unit
  val last_exn : 'a t -> 'a
  val sort : 'a t -> compare:('a -> 'a -> int) -> unit
end

module type Vec = sig
  module Make (Arg : Elt) : S with module Elt = Arg
  module Value : S_value
end
