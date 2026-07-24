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

(** The outcome of {!solve}. Distinct from {!Solver_result.t} because quantifier
    reasoning cannot in general certify satisfiability: a [Sat] here is a
    genuine, proven model, whereas [Unknown_but_possibly_sat] is a ground model
    of everything instantiated so far that may still violate an uninstantiated
    universal. *)
module Result : sig
  type t =
    | Unsat of
        { core : Solver_result.Core_step.t list
        ; proof : Proof.t option [@sexp.option]
        }
    | Sat of { model : Model.t }
    | Unknown_but_possibly_sat of { model : Model.t }
  [@@deriving sexp_of]
end

(** Repeatedly solves the underlying [Solver.t], then e-matches every registered
    axiom's triggers against the current egraph and asserts newly-found ground
    instances (guarded by the axiom's guard atom, so it's sound regardless of
    whether that atom happens to be forced true), until one of:
    - the SAT search reports [Unsat] -> [Result.Unsat] (authoritative)
    - a ground model is reached (either a round finds no new instances, or
      [max_rounds] -- default 50 -- is hit; E-matching over UF need not
      terminate). Such a model is returned as [Result.Sat] only if no universal
      axioms were ever asserted, so the underlying ground result is
      authoritative; otherwise it is [Result.Unknown_but_possibly_sat], since
      trigger-based instantiation is incomplete -- the model satisfies every
      instance generated so far but is not proven to satisfy the universals over
      terms no trigger matched. *)
val solve
  :  ?time_bound:Feel.Solver.time_bound
  -> ?assumptions:int array
  -> ?max_rounds:int
  -> t
  -> Result.t
