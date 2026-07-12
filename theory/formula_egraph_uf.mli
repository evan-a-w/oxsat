open! Core
open! Feel.Import

(** Congruence closure over the whole [Formula.any] type (not just the
    [Var]/[App] slice {!Uninterpreted_functions} uses), built directly on
    {!Formula_egraph.Graph}. Provides the same justification tracking,
    decision-level undo, and conflict/propagation lemma extraction as
    {!Uninterpreted_functions.S}. *)

module Atom : sig
  type t = [ `Eq of Formula.any * Formula.any ]

  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
  val normalize : t -> t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

type t

val create : atoms:Atom.t list -> t
val add_atom : t -> atom:Atom.t -> unit
val assert_atom : t -> decision_level:int -> atom:Atom.t -> value:bool -> unit
val maybe_get_lemma : t -> [ `Consistent | `Lemma of (Atom.t * bool) list ]
val undo : t -> to_decision_level_excl:int -> unit

(** The representative term of [term]'s equivalence class under the current
    congruence closure. [term] must already be registered (e.g. via [add_atom]
    or [create]). *)
val canonical_term : t -> term:Formula.any -> Formula.any

(** All terms registered so far (via [create] or [add_atom]), including
    subterms. *)
val registered_terms : t -> Formula.any list

(** The underlying e-graph, for e-matching (see {!Formula_egraph.Pattern}) or
    other queries over the shape of registered terms — e.g. for instantiating
    axioms whose trigger pattern currently exists in the graph. *)
val egraph : t -> Formula_egraph.Graph.t
