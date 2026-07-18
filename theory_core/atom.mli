open! Core
open! Feel.Import

(** Le can express all the things we care about when in formulas.
    - [x <= c] is [`Le (Linear_expr.var x, c)]
    - [x >= c] is [`Le (Linear_expr.neg (Linear_expr.var x), Q.neg c)]
    - [x > c] is [Not (Atom (`Le (Linear_expr.var x, c)))]
    - [x < c] is
      [Not (Atom (`Le (Linear_expr.neg (Linear_expr.var x), Q.neg c)))] *)
type t =
  [ `Eq of Formula.any * Formula.any
  | `Le of Linear_expr.t * Q.t
  | `Type_eq of Type_expr.t * Type_expr.t
  ]
[@@deriving sexp]

val normalize : t -> t

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t
