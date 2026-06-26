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
  | Base.Int -> Tvar.of_string ":Int"
  | Base.Float -> Tvar.of_string ":Float"
;;

(* Prefixes [tvar] with [':'] so that, once embedded as a term in the (merged)
   value-level EUF, it cannot collide with a value-level variable or function
   symbol of the same name -- those are never prefixed with [':']. *)
let tvar_for_term tvar = Tvar.of_string (":" ^ Tvar.to_string tvar)

include functor Hashable.Make
include functor Comparable.Make
