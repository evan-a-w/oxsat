open! Core
open! Feel.Import

type t =
  [ `Eq of [ `Uf ] Formula.t * [ `Uf ] Formula.t
  | `Le of Linear_expr.t * Q.t
  | `Type_eq of Type_expr.t * Type_expr.t
  ]

(* [@@deriving] treats [Formula.t]'s phantom parameter like an ordinary type
   argument, so it generates calls assuming [Formula.sexp_of_t]/[compare]/[hash]
   take an extra per-argument converter (as they would for a real container
   type) -- they don't, so these are written by hand instead. *)
let sexp_of_t : t -> Sexp.t = function
  | `Eq (a, b) ->
    [%message
      "Eq" ~_:(Formula.sexp_of_t a : Sexp.t) ~_:(Formula.sexp_of_t b : Sexp.t)]
  | `Le (expr, c) -> [%message "Le" ~_:(expr : Linear_expr.t) ~_:(c : Q.t)]
  | `Type_eq (a, b) ->
    [%message "Type_eq" ~_:(a : Type_expr.t) ~_:(b : Type_expr.t)]
;;

let compare (t1 : t) (t2 : t) =
  match t1, t2 with
  | `Eq (a1, b1), `Eq (a2, b2) ->
    Tuple2.compare ~cmp1:Formula.compare ~cmp2:Formula.compare (a1, b1) (a2, b2)
  | `Le (e1, c1), `Le (e2, c2) ->
    Tuple2.compare
      ~cmp1:[%compare: Linear_expr.t]
      ~cmp2:[%compare: Q.t]
      (e1, c1)
      (e2, c2)
  | `Type_eq (a1, b1), `Type_eq (a2, b2) ->
    Tuple2.compare
      ~cmp1:[%compare: Type_expr.t]
      ~cmp2:[%compare: Type_expr.t]
      (a1, b1)
      (a2, b2)
  | `Eq _, (`Le _ | `Type_eq _) -> -1
  | (`Le _ | `Type_eq _), `Eq _ -> 1
  | `Le _, `Type_eq _ -> -1
  | `Type_eq _, `Le _ -> 1
;;

let hash_fold_t (state : Hash.state) (t : t) : Hash.state =
  match t with
  | `Eq (a, b) ->
    Formula.hash_fold_t (Formula.hash_fold_t (Hash.fold_int state 0) a) b
  | `Le (expr, c) ->
    [%hash_fold: Linear_expr.t * Q.t] (Hash.fold_int state 1) (expr, c)
  | `Type_eq (a, b) ->
    [%hash_fold: Type_expr.t * Type_expr.t] (Hash.fold_int state 2) (a, b)
;;

let hash (t : t) : int = Hash.run hash_fold_t t

let normalize = function
  | `Eq (a, b) as x ->
    (match Ordering.of_int (Formula.compare a b) with
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
