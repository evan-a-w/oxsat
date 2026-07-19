open! Core
open! Feel.Import

module Base = struct
  type t =
    | Bool
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
  | Function_type of t * t
  | Type
[@@deriving sexp, compare, hash]

include functor Hashable.Make
include functor Comparable.Make

let split_function = function
  | App (function_, args) -> Some (function_, args)
  | Base _ | Var _ | Type_of _ | Function_type _ | Type -> None
;;

let rec to_formula : t -> Formula.any = function
  | Var v -> Type_var v
  | Base Bool -> Bool
  | Base Int -> Int
  | Base Float -> Float
  | Type_of v -> Type_of (Var v)
  | App (f, args) -> Type_app (f, List.map args ~f:to_formula)
  | Type -> Formula.Type
  | Function_type (a, b) -> Function_type (to_formula a, to_formula b)
;;

let garbage_for_vec = Var (Tvar.of_string "")
