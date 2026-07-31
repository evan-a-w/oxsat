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

## Algebraic datatypes follow-ups

- Implement a full non-ground ADT model representation. Current ADT model
  checking is intentionally finite-ground over observed solver terms.

## Nested quantifiers

- Widen quantifier bodies so nested quantifiers and alternation are expressible.
- Implement proper Skolem functions depending on enclosing universal variables;
  do not use fresh ground constants except where sound.
- Preserve scope for term-level ITEs and triggers under nested binders.
- Extend proof support for nested quantifier elaboration where feasible; escalate
  only genuinely hard proof-kernel changes.

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

- Leave full quantifier/e-matching proof production until the end.
- Eventually certify the whole instantiation loop so cached instantiations can be
  trusted artifacts rather than only optimizations.
- Include proof-printing examples and bogus-proof rejection tests for quantified
  proofs.

## ITE follow-ups

- Watch for exponential blowup from case-splitting `Ite` expansion.
- Consider Tseitin-style term ITE temporaries if ITE-heavy formulas or quantified
  ITEs become a bottleneck.
- Document limitations around ITEs in trigger terms and type positions.
- Add extra tests for ITE in linear arithmetic, store arguments, nested ITEs,
  ITE under `Not`/`Or`, skolemized existential bodies, and generated-name sexp
  round trips.
