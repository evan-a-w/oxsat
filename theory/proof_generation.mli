open! Core
open! Import

type t

val create : unit -> t
val assert_formula : t -> Formula.any -> unit
val push : t -> unit
val pop : t -> unit

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
