open! Core
open! Import

(** Turns a possibly-quantified formula into a plain ground formula plus the
    universal axioms extracted along the way, so the existing (unchanged)
    [Solver.assert_formula]/[Encoding.encode] can consume the result.

    Runs NNF (pushing [Not] through [And]/[Or]/[Forall]/[Exists], flipping
    quantifiers under negation). Positive existentials are Skolemized at the
    point where they occur. If the enclosing in-scope universals are
    [[u1; ...; uk]] (outermost first), each existential variable is replaced by
    [App (sk, [Var u1; ...; Var uk])]. When [k = 0], the historical top-level
    form [Var sk] is used instead of [App (sk, [])].

    Positive universals are alpha-renamed and registered as
    {!Quantifier_axiom.Axiom.t}s. A guarded universal nested under boolean
    structure is replaced by a fresh guard atom; instances are later asserted
    guarded by that atom. Currently every guard that NNF elaboration creates is
    positive and can be given a definitional quantified assumption. Within one
    positive universal subtree, all inner universals are prenexed into the same
    axiom, including universals reached through boolean structure, and inner
    existentials are Skolemized using exactly the universals in scope at their
    occurrence. The resulting axiom body is binder-free.

    Trigger groups from hoisted universals are merged by cross-product
    concatenation in preorder: alternatives within one [forall] remain
    alternatives, while groups from distinct hoisted [forall]s are appended so
    each final group contains as many available trigger terms as possible. A
    triggerless [forall] contributes no terms. If no final group covers every
    bound variable, the axiom is inert because the quantifier solver drops
    partial substitutions.

    Quantifiers inside triggers, or inside a non-boolean-skeleton position (e.g.
    as an argument to [Eq]/[App]), remain out of scope for elaboration even
    though the [Formula.quantified] type can represent them. *)
val elaborate
  :  Formula.quantified
  -> Formula.any * Quantifier_axiom.Axiom.t list

(** Registers a {b top-level, binder-free} universal: alpha-renames [bound] to
    fresh variables and returns a guard-free ({!Quantifier_axiom.Axiom.guard} =
    [None]) axiom together with the renamed [∀] itself, for a proof to cite as
    an assumption and justify each instance by universal instantiation. Unlike
    {!elaborate}, imposes no ground constraint and no guard atom. *)
val register_toplevel_forall
  :  bound:Tvar.t list
  -> triggers:Formula.any list list
  -> body:Formula.any
  -> Quantifier_axiom.Axiom.t * Formula.quantified

module Toplevel_prefix : sig
  type t =
    { given : Formula.quantified
    ; ground : Formula.any option
    ; axiom : Quantifier_axiom.Axiom.t option
    }
end

(** Registers a top-level quantifier prefix whose body is binder-free.
    Universals become a guard-free axiom; existentials are Skolemized in the
    solver axiom with dependencies on the universals in scope. The returned
    [given] is the alpha-renamed formula to cite in proofs. *)
val register_toplevel_prefix
  :  Formula.quantified
  -> Toplevel_prefix.t Or_error.t

(** Skolemizes a {b top-level} existential's [body]: replaces each bound
    variable with a fresh ground constant, returning the witnessing substitution
    (bound variable to its Skolem constant) alongside the resulting ground body.
    The substitution is what a proof's existential-elimination step records.
    Nested elaboration uses Skolem functions internally, but this proof-path
    helper intentionally keeps the historical zero-universal semantics. *)
val skolemize_existential
  :  bound:Tvar.t list
  -> Formula.any
  -> (Tvar.t * Formula.any) list * Formula.any
