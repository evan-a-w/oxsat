# Future plans

## Arithmetic / big integers

- Integrate the standalone `ds` big integer module into `theory_core/Q.t`.
- Preserve the current small-int fast path if possible, but make every rational
  operation overflow-safe.
- Add rational tests around values outside OCaml `int` / machine-integer bounds,
  especially `2^63` and C-like overflow limits.
- Re-run arithmetic, simplex, branch-and-bound, SMT, and proof tests after Q is
  migrated.
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

Remaining:

- `push`/`pop` scope activation literals still yield `proof = None`; that path
  is untouched and is now the only remaining decline.
- Quantified *conclusions* (a lemma `∀x. P(x)`, not just a quantified
  assumption) need universal generalization — `Forall_intro` with an
  eigenvariable condition over the step DAG's dependency closure, plus the cheap
  `Exists_intro`. This is the fork toward a real proof language; Route A proofs
  stay valid verbatim under it.
- Eventually certify the whole instantiation loop so cached instantiations can be
  trusted artifacts rather than only optimizations.

## ITE follow-ups

- Watch for exponential blowup from case-splitting `Ite` expansion.
- Consider Tseitin-style term ITE temporaries if ITE-heavy formulas or quantified
  ITEs become a bottleneck.
- Document limitations around ITEs in trigger terms and type positions.
- Add extra tests for ITE in linear arithmetic, store arguments, nested ITEs,
  ITE under `Not`/`Or`, skolemized existential bodies, and generated-name sexp
  round trips.
