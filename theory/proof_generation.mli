open! Core
open! Import

type t

val create : unit -> t
val assert_formula : t -> Formula.any -> unit
val push : t -> unit
val pop : t -> unit

(** Records a top-level [∀]/[∃] as an assumption the proof may cite. *)
val add_quantified_given : t -> Formula.quantified -> unit

(** Records that ground [instance] was asserted as a consequence of the (already
    {!add_quantified_given}-registered) universal [forall] under [bound_values],
    so the proof derives it by universal instantiation rather than assuming it. *)
val note_forall_instance
  :  t
  -> instance:Formula.any
  -> forall:Formula.quantified
  -> bound_values:(Tvar.t * Formula.any) list
  -> unit

(** Like {!note_forall_instance} but for the ground [skolem_body] an existential
    was Skolemized to; the proof derives it by existential elimination. *)
val note_exists_skolemization
  :  t
  -> skolem_body:Formula.any
  -> existential:Formula.quantified
  -> skolems:(Tvar.t * Formula.any) list
  -> unit

(** Marks a ground atom as having no real meaning (a guard for a universal
    nested inside boolean structure). {!unsat_proof} declines (returns [None])
    when a refutation depends on such an atom. *)
val note_synthetic : t -> Formula.any -> unit

(** Assembles a checked refutation proof of the current assertions from the SAT
    core's resolution trace ([refutation_clauses], from
    {!Feel.Solver.last_refutation}), the {!Encoding} (for atoms and Tseitin
    definitions), a lookup from a lemma clause's atoms to its theory
    certificate, and the map from unit-clause literals back to the formulas they
    assert.

    Returns [None] when the refutation depends on a [push]/[pop] scope's
    activation literal (one of [scope_vars]), whose proof modeling is not yet
    supported. Raises if a proof it does attempt fails to check -- for a
    supported unsat, full production must yield a checkable proof. *)
val unsat_proof
  :  t
  -> encoding:Encoding.t
  -> certificate_for_atoms:(Atom.t list -> Lemma_certificate.t option)
  -> formula_by_root_lit:(int, Formula.any) Hashtbl.t
  -> scope_vars:int list
  -> refutation_clauses:Feel.Solver.Refutation_clause.t list
  -> Proof.t option
