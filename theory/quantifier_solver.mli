open! Core
open! Import

(** A layer on top of {!Solver.t} (not a change to it) that adds
    foralls/existentials: {!assert_formula} elaborates a possibly-quantified
    formula (see {!Quantifier_elaboration}) before handing the resulting ground
    formula to the underlying, unchanged [Solver.assert_formula], and {!solve}
    automatically drives a solve -> e-match -> assert -> re-solve loop over the
    axioms extracted along the way. *)
type t

val create : ?config:Solver.Config.t -> unit -> t

(** Elaborates [formula] (Skolemizing existentials, registering universal
    axioms) and asserts the resulting ground formula via the underlying
    [Solver.assert_formula]. See {!Solver.assert_formula} for the return value's
    meaning. *)
val assert_formula
  :  t
  -> Formula.quantified
  -> [ `Ok | `Unsat of Feel.Sat_result.Core_clause.t list ] Or_error.t

val push : t -> unit
val pop : t -> unit

(** The underlying theory egraph -- see {!Solver.egraph}. *)
val egraph : t -> Formula_egraph_uf.t

(** Repeatedly solves the underlying [Solver.t], then e-matches every registered
    axiom's triggers against the current egraph and asserts newly-found ground
    instances (guarded by the axiom's guard atom, so it's sound regardless of
    whether that atom happens to be forced true), until one of:
    - the SAT search reports [Unsat]
    - a round finds no new instances (saturation -- the returned [Sat] is a
      genuine model, modulo the inherent incompleteness of E-matching-based
      quantifier reasoning: the absence of a matching ground term doesn't prove
      the axiom's universal validity)
    - [max_rounds] (default 50) is reached, since E-matching over UF need not
      terminate -- in that case the returned [Sat] model is only as good as the
      instances found so far. *)
val solve
  :  ?time_bound:Feel.Solver.time_bound
  -> ?assumptions:int array
  -> ?max_rounds:int
  -> t
  -> Solver_result.t
