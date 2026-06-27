open! Core
open! Feel.Import

(** General atomic formulas shared across theories: equality between [Term.t]s
    (the atom kind used by the theory of uninterpreted functions), and
    non-strict linear inequalities [expr <= c] (used by [Simplex]). The other
    inequality directions and strict inequalities are expressed via
    [Formula.Not] and by negating the linear expression:

    - [x <= c] is [`Le (Linear_expr.var x, c)]
    - [x >= c] is [`Le (Linear_expr.neg (Linear_expr.var x), Q.neg c)]
    - [x > c] is [Not (Atom (`Le (Linear_expr.var x, c)))]
    - [x < c] is
      [Not (Atom (`Le (Linear_expr.neg (Linear_expr.var x), Q.neg c)))] *)
type t =
  [ `Eq of Term.t * Term.t
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
