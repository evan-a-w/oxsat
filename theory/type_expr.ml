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
  | App of Tvar.t * t list
[@@deriving sexp, compare, hash]

let base_tvar = function
  | Base.Int -> Tvar.of_string "__Int__"
  | Base.Float -> Tvar.of_string "__Float__"
;;

include functor Hashable.Make
include functor Comparable.Make
