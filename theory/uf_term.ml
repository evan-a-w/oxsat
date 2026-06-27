open! Core
open! Feel.Import

type t =
  [ `App of function_:Tvar.t * args:t list
  | `Var of Tvar.t
  ]
[@@deriving sexp, compare, hash]

include functor Comparable.Make
include functor Hashable.Make

let split_function = function
  | `App (~function_, ~args) -> Some (function_, args)
  | `Var _ -> None
;;

let garbage_for_vec = `Var (Tvar.of_string "")

module Uf = Uninterpreted_functions.Make (struct
    type nonrec t = t [@@deriving sexp, compare, hash]

    let split_function = split_function
    let garbage_for_vec = garbage_for_vec

    include functor Comparable.Make
    include functor Hashable.Make
  end)
