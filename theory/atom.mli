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

(** For [`Eq], orders the two sides canonically so that semantically identical
    atoms compare equal regardless of argument order.

    For [`Le (expr, c)], rewrites to the canonical primitive form (via
    [Linear_expr.primitive]) with the constant moved entirely to the right-hand
    side, so that e.g. [2x+2y<=4] and [x+y<=2] normalize to the same atom. *)
val normalize : t -> t

include Comparable.S with type t := t
include Hashable.S with type t := t
