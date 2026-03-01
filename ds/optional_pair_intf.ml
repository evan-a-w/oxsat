open! Core

module type Elt = sig
  type t

  val trivial_create_for_none : unit -> t
end

module type S = sig
  module Fst : Elt
  module Snd : Elt

  type t = (Fst.t * Snd.t) option

  val none : unit -> t
  val some : Fst.t * Snd.t -> t
  val is_none : t -> bool
  val is_some : t -> bool
  val value : t -> default:(unit -> Fst.t * Snd.t) -> Fst.t * Snd.t
  val value_exn : t -> Fst.t * Snd.t

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool
      val unsafe_value : t -> Fst.t * Snd.t
    end
  end
end

module type Optional_pair = sig
  module type S = S

  module Make (A : Elt) (B : Elt) : S with module Fst = A and module Snd = B
end
