open! Core
open! Feel.Import

type t =
  [ `App of function_:Tvar.t * args:t list
  | `Var of Tvar.t
  ]
[@@deriving sexp, compare, hash]

include Comparable.S with type t := t
include Hashable.S with type t := t
