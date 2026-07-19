open! Core
open! Feel.Import

(** Ground base types. *)
module Base : sig
  type t =
    | Bool
    | Int
    | Float
  [@@deriving sexp, compare, equal, hash, enumerate]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

type t =
  | Var of Tvar.t
  | Base of Base.t
  | Type_of of Tvar.t
  | App of Tvar.t * t list
  | Function_type of t * t
  | Type
[@@deriving sexp, compare, hash]

include Comparable.S with type t := t
include Hashable.S with type t := t

val split_function : t -> (Tvar.t * t list) option

(** Widens a type expression to the [Formula.any] term the egraph and proof
    checker reason over, mapping [Var] to [Type_var] so it stays distinct from a
    UF-role [Var]. *)
val to_formula : t -> Formula.any

(** for vec *)
val garbage_for_vec : t
