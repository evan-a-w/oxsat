open! Core
open! Feel.Import
module Type = Type_expr.Base

module Atom : sig
  type t = [ `Has_type of Tvar.t * Type_expr.t ]
  [@@deriving sexp, compare, hash]

  val normalize : t -> t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

type t

val create : unit -> t
val get_type : t -> Tvar.t -> Type_expr.t option
val assert_atom : t -> decision_level:int -> atom:Atom.t -> value:bool -> unit
val maybe_get_lemma : t -> [ `Consistent | `Lemma of (Atom.t * bool) list ]
val undo : t -> to_decision_level_excl:int -> unit
