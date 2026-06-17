open! Core
open! Feel.Import

(** An incremental SMT solver combining the SAT core ({!Feel.Solver}) with
    the theory of uninterpreted functions ({!Uninterpreted_functions}).

    Formulas are asserted via {!assert_formula} and Tseitin-encoded into CNF
    using a shared {!Formula.Encoding.t}, so atoms keep a stable mapping to
    SAT variables across calls. {!push}/{!pop} provide assertion scopes,
    implemented via activation literals: clauses asserted within a scope are
    only enforced while that scope (and all enclosing scopes) are active. *)
type t

val create : unit -> t

(** Asserts that [formula] is true. May be called between [solve] calls
    (including before the first one) to add new constraints incrementally.
    Any new theory atoms appearing in [formula] are registered with the
    underlying theory.

    Returns [`Unsat unsat_core] if this assertion makes the (currently
    active scopes of the) formula unsatisfiable at the SAT level
    independent of any future [solve] call -- in that case the offending
    clause was not enforced, mirroring {!Feel.Solver.add_clause}. *)
val assert_formula : t -> Formula.t -> [ `Ok | `Unsat of int array ]

(** Opens a new assertion scope. Formulas asserted after [push] (until the
    matching [pop]) are only enforced while this scope is active. *)
val push : t -> unit

(** Closes the innermost open scope, retracting the SAT-level constraints
    asserted within it (they become permanently inert). Theory atoms and
    terms registered while the scope was open remain known to the theory --
    [pop] retracts constraints, not vocabulary. *)
val pop : t -> unit

(** [solve t] checks satisfiability of all formulas asserted in currently
    active scopes. [assumptions] are additional unit assumptions for this
    call only, combined with the activation literals of any open scopes. *)
val solve
  :  ?time_bound:Feel.Solver.time_bound
  -> ?assumptions:int array
  -> t
  -> Feel.Sat_result.t

val stats : t -> Feel.Stats.t
val assert_type : t -> Tvar.t -> Tvar_types.Type.t -> unit
val get_type : t -> Tvar.t -> Tvar_types.Type.t option
