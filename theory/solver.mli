open! Core
open! Import

(** An incremental SMT solver combining the SAT core ({!Feel.Solver}) with
    congruence closure over terms and types ({!Formula_egraph_uf}), linear
    arithmetic ({!Branch_and_bound}), and the type theory ({!Tvar_types}).

    Formulas are asserted via {!assert_formula} and Tseitin-encoded into CNF
    using a shared {!Encoding.t}, so atoms keep a stable mapping to SAT
    variables across calls. {!push}/{!pop} provide assertion scopes, implemented
    via activation literals: clauses asserted within a scope are only enforced
    while that scope (and all enclosing scopes) are active. *)
type t

module Config : sig
  type t = { produce_proofs : bool } [@@deriving sexp_of]

  val default : t
end

val create : ?config:Config.t -> unit -> t

(** Asserts that [formula] is true. May be called between [solve] calls
    (including before the first one) to add new constraints incrementally. Any
    new theory atoms appearing in [formula] are registered with the underlying
    theory.

    Returns [`Unsat core] if this assertion makes the (currently active scopes
    of the) formula unsatisfiable at the SAT level independent of any future
    [solve] call -- in that case the offending clause was not enforced,
    mirroring {!Feel.Solver.add_clause}.

    Returns an [Error] if [formula] is ill-formed (see {!Encoding.encode}). *)
val assert_formula
  :  t
  -> Formula.any
  -> [ `Ok | `Unsat of Feel.Sat_result.Core_clause.t list ] Or_error.t

(** Opens a new assertion scope. Formulas asserted after [push] (until the
    matching [pop]) are only enforced while this scope is active. *)
val push : t -> unit

(** Closes the innermost open scope, retracting the SAT-level constraints
    asserted within it (they become permanently inert). Theory atoms and terms
    registered while the scope was open remain known to the theory -- [pop]
    retracts constraints, not vocabulary. *)
val pop : t -> unit

val stats : t -> Feel.Stats.t

(** Checks satisfiability of all formulas asserted in currently active scopes.
    [assumptions] are additional unit assumptions for this call only. On [Sat],
    [model] carries the boolean value assigned to each theory atom together
    with, for each [Tvar.t] known to some theory, whatever that theory
    determined about it (type, numeric value, and/or EUF equivalence-class
    representative) -- enough for {!check_model} to re-verify it. On [Unsat],
    [core] contains the conflicting assertions and theory lemmas, with Tseitin
    variables resolved back to their formulas. When proof production is enabled,
    [proof] contains a checked refutation proof. *)
val solve
  :  ?time_bound:Feel.Solver.time_bound
  -> ?assumptions:int array
  -> t
  -> Solver_result.t

(** Independently verifies a [Sat] model against the currently asserted formulas
    (see {!Model.check}). Errors if any asserted formula is not forced true by
    the model or an atom value contradicts its numeric/type witness. *)
val check_model : t -> Model.t -> unit Or_error.t

val assert_type : t -> Tvar.t -> Type_expr.t -> unit
val get_type : t -> Tvar.t -> Type_expr.t option

(** The theory egraph, e.g. for e-matching quantified axioms' trigger patterns
    against the current ground terms and asserting the resulting instances via
    {!assert_formula}. Every asserted formula's whole shape is registered (see
    {!Formula_egraph_uf.add_term}) — lazily, on this call, so solving doesn't
    pay for the extra congruence-closure nodes; call again after new assertions
    to index them. *)
val egraph : t -> Formula_egraph_uf.t
