open! Core
open! Feel.Import

type t =
  [ `Eq of Term.t * Term.t
  | `Le of Linear_expr.t * Q.t
  | `Has_type of Tvar.t * Type_expr.t
  | `Type_eq of Type_expr.t * Type_expr.t
  ]
[@@deriving sexp, compare, hash]

let normalize = function
  | `Eq (a, b) as x ->
    (match Ordering.of_int ([%compare: Term.t] a b) with
     | Equal | Less -> x
     | Greater -> `Eq (b, a))
  | `Le (expr, c) ->
    let full, _factor = Linear_expr.(primitive (expr - const c)) in
    `Le ({ full with const = Q.zero }, Q.neg full.const)
  | `Has_type _ as x -> x
  | `Type_eq (te1, te2) as x ->
    (match Ordering.of_int ([%compare: Type_expr.t] te1 te2) with
     | Equal | Less -> x
     | Greater -> `Type_eq (te2, te1))
;;

include functor Comparable.Make
include functor Hashable.Make
