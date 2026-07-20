open! Core
open! Import

(** Congruence closure over the whole [Formula.any] term language, built
    directly on {!Formula_egraph.Graph}, with justification tracking,
    decision-level undo, and conflict/propagation lemma extraction.

    Handles both kinds of equality atom natively: a [`Type_eq]'s endpoints are
    embedded into [Formula.any] (see {!Atom.Equality.endpoints}) for term
    registration, but the atom itself is kept as-is, so lemmas and certificates
    cite the same atoms the encoding registered. *)

type t

val create : atoms:Atom.Equality.t list -> t
val add_atom : t -> atom:Atom.Equality.t -> unit

(** Registers [term] (and all its subterms) in the egraph without tying it to
    any atom, e.g. a whole asserted formula, so e-matching (see
    {!Formula_egraph.Pattern}) can find its shapes when instantiating quantified
    axioms. Inert with respect to lemma extraction: only atoms drive conflicts
    and propagations. *)
val add_term : t -> term:Formula.any -> unit

val assert_atom
  :  t
  -> decision_level:int
  -> atom:Atom.Equality.t
  -> value:bool
  -> unit

val maybe_get_lemma
  :  t
  -> [ `Consistent | `Lemma of (Atom.Equality.t * bool) list ]

(** The EUF proof certificate for the [`Lemma] most recently returned by
    {!maybe_get_lemma}, or [None] if that call returned [`Consistent]. Used by
    proof production; the returned certificate justifies the lemma clause over
    its constituent equality atoms. *)
val last_certificate : t -> Lemma_certificate.Euf.t option

val undo : t -> to_decision_level_excl:int -> unit

(** The current truth value of [atom], if it has been assigned one (via
    {!assert_atom}) and not since undone. [None] if [atom] hasn't been
    registered or hasn't been assigned. *)
val atom_value : t -> atom:Atom.Equality.t -> bool option

(** Whether [term] has been registered, whether via an atom or {!add_term}. *)
val mem_term : t -> Formula.any -> bool

(** Whether [term] occurs in some registered atom (as an endpoint or a subterm
    of one). Unlike terms only registered via {!add_term}, these can influence
    an atom's truth value. *)
val mem_atom_term : t -> Formula.any -> bool

(** The representative term of [term]'s equivalence class under the current
    congruence closure. [term] must already be registered (e.g. via [add_atom]
    or [create]). *)
val canonical_term : t -> term:Formula.any -> Formula.any

(** All terms registered so far, including subterms. *)
val registered_terms : t -> Formula.any list

(** Every registered term paired with the canonical representative of its
    equivalence class, so a model checker can decide any equality/disequality
    over registered terms without re-running congruence closure. *)
val classes : t -> (Formula.any * Formula.any) list

(** The underlying e-graph, for e-matching (see {!Formula_egraph.Pattern}) or
    other queries over the shape of registered terms — e.g. for instantiating
    axioms whose trigger pattern currently exists in the graph. *)
val egraph : t -> Formula_egraph.Graph.t

(** The term that introduced [id] into the graph (e.g. for reading back an
    e-matching {!Formula_egraph.Pattern.Subst}). *)
val term_of_id : t -> Formula_egraph.Graph.Id.t -> Formula.any option
