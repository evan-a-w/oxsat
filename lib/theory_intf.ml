open! Core

module type S = sig
  type t

  val create : unit -> t
  val assert_literal : t -> decision_level:int -> literal:int -> unit

  val maybe_get_lemma
    :  t
    -> [ `Consistent | `Lemma of int array Modes.Global.t ] @ local

  val undo : t -> to_decision_level:int -> unit
  val on_new_var : t -> var:int -> unit
end

module type Theory = sig
  module type S = S

  module Packed : sig
    type t = T : (module S with type t = 's) * 's -> t
  end

  val pack : (module S with type t = 'a) -> 'a -> Packed.t
end
