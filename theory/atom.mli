open! Core
open! Feel.Import

(** Le can express all the things we care about when in formulas.
    - [x <= c] is [`Le (Linear_expr.var x, c)]
    - [x >= c] is [`Le (Linear_expr.neg (Linear_expr.var x), Q.neg c)]
    - [x > c] is [Not (Atom (`Le (Linear_expr.var x, c)))]
    - [x < c] is
      [Not (Atom (`Le (Linear_expr.neg (Linear_expr.var x), Q.neg c)))] *)
type t =
  [ `Eq of [ `Uf ] Formula.t * [ `Uf ] Formula.t
  | `Le of Linear_expr.t * Q.t
  | `Type_eq of Type_expr.t * Type_expr.t
  ]

(* Written by hand rather than [@@deriving]: ppx treats [Formula.t]'s phantom
   parameter like an ordinary type argument, so it would generate calls assuming
   [Formula.sexp_of_t]/[compare]/[hash] take an extra per-argument converter (as
   they would for a real container type) -- they don't. *)
val sexp_of_t : t -> Sexp.t
val normalize : t -> t

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t
