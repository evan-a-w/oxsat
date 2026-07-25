open! Core
open! Import

(** Turns a possibly-quantified formula into a plain ground formula plus the
    universal axioms extracted along the way, so the existing (unchanged)
    [Solver.assert_formula]/[Encoding.encode] can consume the result.

    Runs NNF (pushing [Not] through [And]/[Or]/[Forall]/[Exists], flipping
    quantifiers under negation), then bottom-up replaces every quantifier node
    with a ground [Formula.any]:
    - [Exists (bound, body)]: Skolemized eagerly -- each bound var is replaced
      by a fresh ground constant substituted into [body], which is already
      ground (ready to splice in directly; ['Forall]/[Exists] bodies can't
      themselves contain a further quantifier, so there's nothing left to
      eliminate).
    - [Forall (bound, triggers, body)]: alpha-renamed to fresh bound vars
      (capture avoidance) and registered as a {!Quantifier_axiom.Axiom.t}; a
      fresh guard atom -- an ordinary, otherwise-unconstrained [Eq] between two
      fresh ground vars -- is spliced into the tree in its place. A
      [Forall]/[Exists] with no triggers (including one produced by NNF flipping
      a triggerless [Exists]) is registered but inert: with nothing to e-match
      on it is never instantiated.

    Quantifiers nested inside a non-boolean-skeleton position (e.g. as an
    argument to [Eq]/[App]) are out of scope: such a term is treated as an
    opaque, already-ground atom. This isn't reachable through any sensible use
    of [Forall]/[Exists] (comparing a term to a formula, or applying a function
    to one, is meaningless), so it's a named limitation rather than a silent
    gap. *)
val elaborate
  :  Formula.quantified
  -> Formula.any * Quantifier_axiom.Axiom.t list

(** Registers a {b top-level} universal: alpha-renames [bound] to fresh
    variables and returns a guard-free ({!Quantifier_axiom.Axiom.guard} =
    [None]) axiom together with the renamed [∀] itself, for a proof to cite as
    an assumption and justify each instance by universal instantiation. Unlike
    {!elaborate}, imposes no ground constraint and no guard atom. *)
val register_toplevel_forall
  :  bound:Tvar.t list
  -> triggers:Formula.any list list
  -> body:Formula.any
  -> Quantifier_axiom.Axiom.t * Formula.quantified

(** Skolemizes an existential's [body]: replaces each bound variable with a
    fresh ground constant, returning the witnessing substitution (bound variable
    to its Skolem constant) alongside the resulting ground body. The
    substitution is what a proof's existential-elimination step records. *)
val skolemize_existential
  :  bound:Tvar.t list
  -> Formula.any
  -> (Tvar.t * Formula.any) list * Formula.any
