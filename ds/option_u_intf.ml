open! Core

module type Elt = sig
  type t

  val trivial_create_for_none : unit -> t
end

module type S = sig
  module Elt : Elt

  type t = Elt.t option

  val none : unit -> t
  val some : Elt.t -> t
  val is_none : t -> bool
  val is_some : t -> bool
  val value : t -> default:(unit -> Elt.t) -> Elt.t
  val value_exn : t -> Elt.t

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool
      val unsafe_value : t -> Elt.t
    end
  end
end

module type Option_u = sig
  module type S = S

  module Make (Arg : Elt) : S with module Elt = Arg
end
