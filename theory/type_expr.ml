open! Core
open! Feel.Import

module Base = struct
  type t =
    | Int
    | Float
  [@@deriving sexp, compare, equal, hash, enumerate]

  include functor Hashable.Make
  include functor Comparable.Make
end

type t =
  | Var of Tvar.t
  | Base of Base.t
  | Type_of of Tvar.t
  | App of Tvar.t * t list
[@@deriving sexp, compare, hash]

include functor Hashable.Make
include functor Comparable.Make

let split_function = function
  | App (function_, args) -> Some (function_, args)
  | Base _ | Var _ | Type_of _ -> None
;;

let garbage_for_vec = Var (Tvar.of_string "")

module Uf = Uninterpreted_functions.Make (struct
    type nonrec t = t [@@deriving sexp, compare, hash]

    let split_function = split_function
    let garbage_for_vec = garbage_for_vec

    include functor Comparable.Make
    include functor Hashable.Make
  end)
