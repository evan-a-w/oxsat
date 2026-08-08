# Future plans

## Arithmetic / big integers

Done: `theory_core/Q.t` now stores numerator/denominator as arbitrary-precision
`Bigint.t`, which is required for exact Int64 bounds.

Remaining:

- Preserve the current small-int fast path if performance regresses.
- Add more rational stress tests around very large values and C-like overflow
  limits.
- Benchmark the rational migration; if performance regresses, consider a hybrid
  small/big representation.

## Deterministic resource budgets

- Replace or supplement wall-clock `time_bound` with deterministic budgets:
  conflicts, propagations, decisions, theory lemma rounds, and quantifier
  instantiation rounds.
- Report consumed budget in solver results/stats so builds can warn when proofs
  are close to their limit.
- Keep wall-clock timeout as an emergency escape hatch, not the reproducibility
  mechanism.

## Nested quantifiers

Done: quantifier bodies/triggers now carry the same phantom tag as the enclosing
formula, so nesting and alternation are expressible while `Formula.any` stays
statically binder-free. Positive universal subtrees prenex into a single axiom;
existentials become Skolem functions of exactly the universals in scope at their
occurrence (bare constants only at the top level).

Remaining:

- Quantifiers inside triggers, or in a non-boolean-skeleton position (an
  argument to `Eq`/`App`), are still rejected rather than elaborated.
- Trigger groups from hoisted universals merge by cross-product; revisit if that
  proves too coarse in practice.

## Trigger inference and quantifier UX

- Infer standard triggers when none are supplied: minimal UF/ADT/array/application
  subterms covering all bound variables.
- Report triggerless or never-instantiated axioms.
- Improve `Unknown` results with actionable reasons, especially uninstantiated
  axioms and exhausted instantiation/fuel budgets.

## Fuel / recursive definitions

- Add a supported encoding for recursive definitions with explicit fuel or a
  controlled unrolling policy.
- Prevent recursive defining axioms from matching-looping until `max_rounds` by
  accident.
- Report consumed fuel/unroll count and whether a goal is unknown because fuel
  was exhausted.

## Model presentation

- Add readable model values for uninterpreted symbols.
- Include a finite graph for each function symbol over registered/applicable
  arguments.
- Improve counterexample presentation for arrays and ADTs within the documented
  finite-ground universe.

## Quantifier proof production

Done ("Route A"): top-level alternation produces checked proofs with no new
kernel rules. Outer universals are instantiated first, so each `∃` eliminated is
ground and witnessed by a distinct fresh constant per instantiation point.
`Forall_instantiation`/`Exists_elim` may now conclude a still-quantified formula;
substitution over quantified formulas rejects binder capture.

Also done: quantifiers nested under boolean structure produce checked proofs.
Their guard atom is now definitional rather than synthetic — the solver cites
`∀x. ¬g ∨ body`, the prenexed form of `¬g ∨ ∀x. body`, which is equivalent
because `g` is a fresh ground atom sharing no variable with `bound` (checked at
the citation site). Guarded instances then follow by the existing
`Forall_instantiation`; no new kernel rule was needed. Under NNF every guard
that elaboration creates is spliced positively, so no guarded case remains
synthetic; `Guard.Polarity.Negative` exists for the encoding but is currently
unreachable.

Also done: refutations inside `push`/`pop` scopes produce checked proofs. A
scope's activation literal `a` appears only as the `-a` that `guard_clauses`
prepends to in-scope clauses, and `solve` assumes `a` true, so proof
construction strips those negative literals — recovering exactly the clause set
the same assertions would have produced at top level. Every premise stays a real
formula; `a` never enters the proof. A positive occurrence of a scope var in a
refutation clause would fall outside this model, so it raises rather than being
silently stripped. Such a proof establishes unsat *of the current assertion
stack*: popped scopes' formulas are dropped from the premises and are not
citable.

Remaining:

- Eventually certify the whole instantiation loop so cached instantiations can be
  trusted artifacts rather than only optimizations.
- The only remaining `proof = None` path is a refutation depending on a
  synthetic atom; no currently reachable case produces one.

Also done: the kernel can now *prove* quantified conclusions, not just consume
them as assumptions. `Exists_intro` substitutes chosen witnesses into the body
(no side condition — picking a witness is always sound). `Forall_intro` carries
an explicit nested subproof plus `imports` discharging each of the subproof's
assumptions against an earlier outer step; the eigenvariable condition is then
the local check that no eigenvariable occurs in any subproof assumption, so no
DAG reachability or dependency-closure analysis is needed, and nested
introductions compose by checking the same condition at each level. Eigenvariables
are deliberately kept out of the whole-proof Skolem escape check: a subproof's
conclusion must mention them, and the enclosing `Forall_intro` is what binds them.

These rules are kernel-only — the solver refutes and never proves a quantified
conclusion, so nothing in `proof_generation` emits them and no generated proof
changed. They are exercised solely by hand-built proofs in `test_proof.ml`, which
is a weaker signal than the end-to-end validation the refutation rules get; the
rejection tests carry that weight instead.

Remaining toward a usable proof language:

- Nothing generates introduction rules. A surface syntax or API for authoring
  proofs would be the next step if the artifact is meant to be written by hand
  rather than emitted by the solver.
- `to_string_hum` renders `Forall_intro` subproofs as indented nested blocks to
  arbitrary depth, with IDs qualified by the enclosing step (`s1.a0`, `s0.s0.s0`)
  and each imported subproof assumption annotated `[imported from sN]` so a
  reader can see it is discharged rather than assumed free.

## ITE follow-ups

- Watch for exponential blowup from case-splitting `Ite` expansion.
- Consider Tseitin-style term ITE temporaries if ITE-heavy formulas or quantified
  ITEs become a bottleneck.
- Document limitations around ITEs in trigger terms and type positions.
- Add extra tests for ITE in linear arithmetic, store arguments, nested ITEs,
  ITE under `Not`/`Or`, skolemized existential bodies, and generated-name sexp
  round trips.
