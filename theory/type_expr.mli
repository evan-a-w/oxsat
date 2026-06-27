open! Core
open! Feel.Import

(** Ground base types. *)
module Base : sig
  type t =
    | Int
    | Float
  [@@deriving sexp, compare, equal, hash, enumerate]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

type t =
  | Base of Base.t
  | Var of Tvar.t
  | Type_of of Tvar.t
  | App of Tvar.t * t list
[@@deriving sexp, compare, hash]

include Comparable.S with type t := t
include Hashable.S with type t := t

val split_function : t -> (Tvar.t * t list) option

(** for vec *)
val garbage_for_vec : t

module Uf : Uninterpreted_functions_intf.S with type Term.t := t
