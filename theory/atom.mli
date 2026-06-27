open! Core
open! Feel.Import

(** Le can express all the things we care about when in formulas.
    - [x <= c] is [`Le (Linear_expr.var x, c)]
    - [x >= c] is [`Le (Linear_expr.neg (Linear_expr.var x), Q.neg c)]
    - [x > c] is [Not (Atom (`Le (Linear_expr.var x, c)))]
    - [x < c] is
      [Not (Atom (`Le (Linear_expr.neg (Linear_expr.var x), Q.neg c)))] *)
type t =
  [ `Eq of Uf_term.t * Uf_term.t
  | `Le of Linear_expr.t * Q.t
  | `Type_eq of Type_expr.t * Type_expr.t
  ]
[@@deriving sexp, compare, hash]

val normalize : t -> t

include Comparable.S with type t := t
include Hashable.S with type t := t
