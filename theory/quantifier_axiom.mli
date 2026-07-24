open! Core
open! Import

(** A registered universal axiom: [forall bound, body], instantiated by matching
    [triggers] against ground terms in the egraph. [guard] is a fresh,
    otherwise-unconstrained ground equality atom spliced into the surrounding
    ground formula in place of the original [Forall] node -- see
    {!Quantifier_elaboration}. *)
module Axiom : sig
  type t =
    { guard : Formula.any
    ; bound : Tvar.t list
    ; triggers : Formula.any list list
    ; body : Formula.any
    }
  [@@deriving sexp_of]
end

(** Converts a trigger term (built from an axiom's bound vars and ordinary
    ground symbols) into an e-matching search query (see
    {!Formula_egraph.Pattern.Query.search}): a [Var v] for [v] in [bound]
    becomes a pattern variable, everything else becomes an [App] node via
    {!Formula.op}/{!Formula.args}. *)
val query_of_term
  :  bound:Tvar.t list
  -> Formula.any
  -> Formula_egraph.Pattern.Query.t

(** Reads a search match's bindings back into a [Tvar.t]-keyed substitution,
    resolving each pattern variable's bound e-class to the ground term that
    introduced it (see {!Formula_egraph_uf.term_of_id}). [None] if some variable
    in [bound] wasn't bound by the match (shouldn't happen for a match returned
    by a query built from {!query_of_term} with the same [bound], since every
    trigger variable position becomes a query [Var]). *)
val substitution_of_match
  :  Formula_egraph_uf.t
  -> bound:Tvar.t list
  -> Formula_egraph.Pattern.Match.t
  -> Formula.any Tvar.Map.t option
