open! Core
open! Feel.Import

type t =
  [ `Eq of [ `Uf ] Formula.t * [ `Uf ] Formula.t
  | `Le of Linear_expr.t * Q.t
  | `Type_eq of Type_expr.t * Type_expr.t
  ]
[@@deriving sexp_of, compare, hash]

let normalize = function
  | `Eq (a, b) as x ->
    (match Ordering.of_int ([%compare: [ `Uf ] Formula.t] a b) with
     | Equal | Less -> x
     | Greater -> `Eq (b, a))
  | `Le (expr, c) ->
    let full, _factor = Linear_expr.(primitive (expr - const c)) in
    `Le ({ full with const = Q.zero }, Q.neg full.const)
  | `Type_eq (a, b) as x ->
    (match Ordering.of_int ([%compare: Type_expr.t] a b) with
     | Equal | Less -> x
     | Greater -> `Type_eq (b, a))
;;

include functor Comparable.Make_plain
include functor Hashable.Make_plain
