open! Core
open! Feel.Import

type t =
  [ `App of function_:Tvar.t * args:t list
  | `Var of Tvar.t
  ]
[@@deriving sexp, compare, hash]

include functor Comparable.Make
include functor Hashable.Make
