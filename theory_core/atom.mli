open! Core
open! Feel.Import

(** The equality atoms, which the egraph handles natively: a term equality over
    [Formula.any], or a type equality over [Type_expr.t]. *)
module Equality : sig
  type t =
    [ `Eq of Formula.any * Formula.any
    | `Type_eq of Type_expr.t * Type_expr.t
    ]
  [@@deriving sexp]

  val normalize : t -> t

  (** Both endpoints as terms of the shared [Formula.any] term language; a
      [`Type_eq]'s sides are embedded via {!Type_expr.to_formula}. *)
  val endpoints : t -> Formula.any * Formula.any

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

(** Le can express all the things we care about when in formulas.
    - [x <= c] is [`Le (Linear_expr.var x, c)]
    - [x >= c] is [`Le (Linear_expr.neg (Linear_expr.var x), Q.neg c)]
    - [x > c] is [Not (Atom (`Le (Linear_expr.var x, c)))]
    - [x < c] is
      [Not (Atom (`Le (Linear_expr.neg (Linear_expr.var x), Q.neg c)))] *)
type t =
  [ Equality.t
  | `Le of Linear_expr.t * Q.t
  ]
[@@deriving sexp]

val normalize : t -> t

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t
