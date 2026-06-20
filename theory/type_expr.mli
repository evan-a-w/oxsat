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

(** A type expression - a tree of type constructors, type variables, and ground
    base types. Used as the second argument of [Atom.Has_type]. *)
type t =
  | Base of Base.t
  | Var of Tvar.t
  | App of Tvar.t * t list
[@@deriving sexp, compare, hash]

(** Canonical [Tvar.t] representing a ground base type. These are used as
    constant "term variables" when embedding type expressions into the
    type-level [Uninterpreted_functions] instance. *)
val base_tvar : Base.t -> Tvar.t

include Comparable.S with type t := t
include Hashable.S with type t := t
