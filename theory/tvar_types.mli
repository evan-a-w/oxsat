open! Core
open! Import
module Type = Type_expr.Base

module Atom : sig
  type t = [ `Type_eq of Type_expr.t * Type_expr.t ]
  [@@deriving sexp, compare, hash]

  val normalize : t -> t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

(** [Type_eq (Var var, type_expr)], asserting that [var] has type [type_expr]. *)
val has_type : Tvar.t -> Type_expr.t -> Atom.t

type t

val create : unit -> t
val get_type : t -> Tvar.t -> Type_expr.t option
val all_typed_vars : t -> Tvar.t list
val assert_atom : t -> decision_level:int -> atom:Atom.t -> value:bool -> unit
val maybe_get_lemma : t -> [ `Consistent | `Lemma of (Atom.t * bool) list ]
val undo : t -> to_decision_level_excl:int -> unit
