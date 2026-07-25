Filtering runs only benchmarks whose name contains the -only substring.
(Times are masked off, since they vary between runs.)

  $ ../feel_bench.exe -bench smt -only "EUF chain UNSAT (n=50)" -min-iterations 1 -max-iterations 1 -sample-runs 1 | cut -d: -f1
  SMT solver benchmarks (LP, MILP, EUF)
  EUF chain UNSAT (n=50)

Multiple -only flags match any of the substrings:

  $ ../feel_bench.exe -bench smt -only "EUF two-group SAT (n=50)" -only "EUF web (n=20" -min-iterations 1 -max-iterations 1 -sample-runs 1 | cut -d: -f1
  SMT solver benchmarks (LP, MILP, EUF)
  EUF two-group SAT (n=50)
  EUF web (n=20, f=3)

A filter that matches nothing warns:

  $ ../feel_bench.exe -bench smt -only NONEXISTENT -min-iterations 1 2>&1
  SMT solver benchmarks (LP, MILP, EUF):
  Warning: -only filters matched no benchmarks

An unknown -bench value errors:

  $ ../feel_bench.exe -bench bogus 2>&1
  Unknown benchmark: bogus
  Valid options: examples, rb, map, sat, dimacs, dimacs-sat-js, dimacs-sat-js-ocaml, smt, all
  [1]
