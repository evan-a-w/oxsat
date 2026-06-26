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

(** The current type expression of [var] as determined by the SAT solver's most
    recent propagation. Reliable between [solve] calls for globally-asserted
    types (decision level 0); may be stale for scoped assertions. *)
val get_type : t -> Tvar.t -> Type_expr.t option

(** Informs the theory of the current truth value of [atom], as determined by
    the SAT solver. Like {!Feel.Theory.S.assert_literal}, but in terms of an
    atom rather than a SAT literal -- the caller (typically [theory/solver.ml])
    is responsible for mapping SAT literals to atoms. *)
val assert_atom : t -> decision_level:int -> atom:Atom.t -> value:bool -> unit

(** Like {!Feel.Theory.S.maybe_get_lemma}, but the lemma is expressed as a list
    of literals over atoms rather than SAT literals -- the caller is responsible
    for mapping atoms to SAT variables. Each [(atom, polarity)] pair is a
    literal: [(atom, true)] is the positive literal, [(atom, false)] its
    negation. *)
val maybe_get_lemma : t -> [ `Consistent | `Lemma of (Atom.t * bool) list ]

val undo : t -> to_decision_level_excl:int -> unit
