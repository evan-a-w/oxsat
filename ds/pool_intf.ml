open! Core

module type Elt = sig
  type t

  val create_for_pool : unit -> t
end

module type S = sig
  module Elt : Elt

  type t

  val create : ?chunk_size:int -> unit -> t
  val alloc : t -> Ptr.t
  val free : t -> Ptr.t -> unit
  val get : t -> Ptr.t -> Elt.t
  val set : t -> Ptr.t -> Elt.t -> unit
  val iter : t -> f:(Ptr.t -> unit) -> unit
  val outstanding : t -> int
end

module type S_global = sig
  module Elt : Elt

  val alloc : unit -> Ptr.t
  val free : Ptr.t -> unit
  val get : Ptr.t -> Elt.t
  val set : Ptr.t -> Elt.t -> unit
  val iter : f:(Ptr.t -> unit) -> unit
end

module type Pool = sig
  module type S = S

  module Make (Arg : Elt) : S with module Elt = Arg
  module Make_global (Arg : Elt) : S_global with module Elt = Arg
end
