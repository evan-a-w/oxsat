open! Core
open! Feel.Import

type t = [ `Eq of Term.t * Term.t ] [@@deriving sexp, compare, hash]

let normalize = function
  | `Eq (a, b) as x ->
    (match Ordering.of_int ([%compare: Term.t] a b) with
     | Equal | Less -> x
     | Greater -> `Eq (b, a))
;;

include functor Comparable.Make
include functor Hashable.Make
