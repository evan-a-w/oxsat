open! Core
open! Feel.Import

module Equality = struct
  type t =
    [ `Eq of Formula.any * Formula.any
    | `Type_eq of Type_expr.t * Type_expr.t
    ]
  [@@deriving sexp, compare, hash]

  let normalize = function
    | `Eq (a, b) as x ->
      (match Ordering.of_int (Formula.compare_any a b) with
       | Equal | Less -> x
       | Greater -> `Eq (b, a))
    | `Type_eq (a, b) as x ->
      (match Ordering.of_int ([%compare: Type_expr.t] a b) with
       | Equal | Less -> x
       | Greater -> `Type_eq (b, a))
  ;;

  let endpoints = function
    | `Eq (a, b) -> a, b
    | `Type_eq (a, b) -> Type_expr.to_formula a, Type_expr.to_formula b
  ;;

  include functor Comparable.Make_plain
  include functor Hashable.Make_plain
end

type t =
  [ Equality.t
  | `Le of Linear_expr.t * Q.t
  ]
[@@deriving sexp, compare, hash]

let normalize = function
  | #Equality.t as x -> (Equality.normalize x :> t)
  | `Le (expr, c) ->
    let full, _factor = Linear_expr.(primitive (expr - const c)) in
    `Le ({ full with const = Q.zero }, Q.neg full.const)
;;

include functor Comparable.Make_plain
include functor Hashable.Make_plain
