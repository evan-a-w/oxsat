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
    constant "term variables" when embedding type expressions into the (merged)
    [Uninterpreted_functions] instance. Already [':']-prefixed -- see
    {!tvar_for_term}. *)
val base_tvar : Base.t -> Tvar.t

(** [':']-prefixes [tvar], so that it cannot collide with a value-level variable
    or function symbol of the same name when both are embedded as terms in the
    same (merged) [Uninterpreted_functions] instance. Used for the type
    variables and type constructors appearing in a [Type_expr.t] when embedding
    it as a term. *)
val tvar_for_term : Tvar.t -> Tvar.t

include Comparable.S with type t := t
include Hashable.S with type t := t
