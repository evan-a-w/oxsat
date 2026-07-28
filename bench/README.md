# Benchmarking & profiling workflow

## Running benchmarks

```
dune exec --profile=release bench/feel_bench.exe -- -bench smt
```

Useful flags:

- `-only SUBSTRING` (repeatable): only run benchmarks whose name contains the
  substring. E.g. `-bench smt -only "Bin packing (items=15"`.
- `-o FILE`: save results as sexp for later comparison.
- `-min-iterations N`, `-max-iterations N`, `-sample-runs N`: fix iteration
  counts (useful for profiling and for low-variance A/B runs).

Each result line also reports `Alloc` (bytes allocated per iteration) and
`MajGC` (major collections per iteration); allocation changes are often a more
reliable signal than timing noise.

## A/B comparison against the checked-in baselines

`bench/comparisons/` holds baseline results (`smt.sexp`, `sat.sexp`,
`dimacs.sexp`) for the current state of the code. After making a change, run:

```
bench/compare.sh smt                      # full suite vs baseline
bench/compare.sh smt -only "Bin packing"  # subset
```

This runs the suite, then prints a table of time/alloc ratios (after/before,
so > 1x is a regression) sorted by largest regression.

After an intentional performance change, update the baseline:

```
dune exec --profile=release bench/feel_bench.exe -- -bench smt \
  -o bench/comparisons/smt.sexp
dune exec --profile=release bench/feel_bench.exe -- -bench sat -sat-max-n 80 \
  -o bench/comparisons/sat.sexp
dune exec --profile=release bench/feel_bench.exe -- -bench dimacs \
  -min-iterations 1 -max-iterations 1 -sample-runs 1 \
  -o bench/comparisons/dimacs.sexp
```

Baselines are machine-dependent; only update them on the machine you compare
on. You can also compare any two files manually (`-only` filters both files):

```
dune exec bench/feel_bench.exe -- [-only SUB]... before.sexp after.sexp
```

## CPU profiling

`bench/profile.sh [--profiler flamegraph|samply] [profiler opts] -- [feel_bench args]`
profiles a benchmark. The script defaults to `-min-iterations 50
-max-iterations 50 -sample-runs 5` so the profile captures the benchmark
itself; override after `--` as needed. Both profilers work on Linux and macOS.

flamegraph (default; `cargo install flamegraph`) writes an interactive svg;
open it in a browser yourself. Uses perf on Linux and xctrace (Time Profiler)
on macOS; no root needed on either.

```
bench/profile.sh -- -bench smt -only "Bin packing (items=15"
bench/profile.sh -o bench/binpacking.svg -- -bench smt -only "Bin packing"
open bench/flamegraph.svg
```

perf (Linux only, no cargo needed) records `bench/perf.data` directly. Useful
where flamegraph/samply aren't installed. On WSL2 the `perf` on PATH is often a
stub that doesn't match the running kernel; point `PERF` at a real one:

```
PERF=/usr/lib/linux-tools-5.15.0-186/perf \
  bench/profile.sh --profiler perf --text -- -bench smt -only EUF
```

samply (`cargo install samply`) records a profile and opens the Firefox
Profiler UI in your browser automatically. No root needed.

```
bench/profile.sh --profiler samply -- -bench smt -only "Bin packing (items=15"
# re-open the saved profile later:
samply load bench/samply.json.gz
# record without opening the browser:
bench/profile.sh --profiler samply -s -- -bench smt -only "Bin packing"
```

### Textual profiles (for reading without a browser / AI agents)

`--text` (flamegraph and perf) additionally dumps the folded stacks to
`bench/flamegraph.folded` and prints a flat profile table (also saved to
`bench/flamegraph.txt`):

```
bench/profile.sh --text -- -bench smt -only "Bin packing (items=15"
```

Output columns: `self%` (samples where the function is the leaf), `tot%`
(samples where it appears anywhere on the stack), plus raw sample counts.
Functions are mangled OCaml symbols, e.g. `camlTheory__Simplex__pivot_271`.

You can also summarize any folded-stacks file directly:

```
bench/folded_summary.sh bench/flamegraph.folded
```

## Allocation profiling (memtrace)

Requires `opam install memtrace` (optional; the build works without it).

```
OXSAT_MEMTRACE=bench/trace.ctf dune exec --profile=release bench/feel_bench.exe -- \
  -bench smt -only "Bin packing" -min-iterations 50 -max-iterations 50
memtrace-viewer bench/trace.ctf   # serves a flamegraph UI in the browser
```

Record notable results at the top of the file for your platform and benchmark
under `bench/results/`: `bench/results/{mac,windows}/{sat_dimacs,smt}.txt`.

## Tests

`bench/cram/` contains cram tests (run via `dune build @runtest`) covering
benchmark filtering, saving/comparing results, the folded-stack summarizer,
and profile.sh argument validation. Profiling itself (flamegraph/samply) is
not exercised in tests since it needs perf/xctrace.
