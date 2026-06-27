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
  | Base of Base.t
  | Var of Tvar.t
  | Type_of of Tvar.t
  | App of Tvar.t * t list
[@@deriving sexp, compare, hash]

include functor Hashable.Make
include functor Comparable.Make
