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

## A/B comparison

```
# baseline
dune exec --profile=release bench/feel_bench.exe -- -bench smt -o bench/results/before.sexp
# ... make a change ...
dune exec --profile=release bench/feel_bench.exe -- -bench smt -o bench/results/after.sexp
# compare (sorted by largest time regression; ratios are after/before)
dune exec bench/feel_bench.exe -- bench/results/before.sexp bench/results/after.sexp
```

## CPU profiling

`bench/profile.sh [--profiler flamegraph|samply] [profiler opts] -- [feel_bench args]`
profiles a benchmark. The script defaults to `-min-iterations 50
-max-iterations 50 -sample-runs 5` so the profile captures the benchmark
itself; override after `--` as needed. Both profilers work on Linux and macOS.

flamegraph (default; `cargo install flamegraph`) writes an interactive svg;
open it in a browser yourself. On macOS the workload is re-run under sudo
(dtrace requires root); on Linux it uses perf.

```
bench/profile.sh -- -bench smt -only "Bin packing (items=15"
bench/profile.sh -o bench/binpacking.svg -- -bench smt -only "Bin packing"
open bench/flamegraph.svg
```

samply (`cargo install samply`) records a profile and opens the Firefox
Profiler UI in your browser automatically. No root needed.

```
bench/profile.sh --profiler samply -- -bench smt -only "Bin packing (items=15"
# re-open the saved profile later:
samply load bench/samply.json.gz
```

## Allocation profiling (memtrace)

Requires `opam install memtrace` (optional; the build works without it).

```
OXSAT_MEMTRACE=bench/trace.ctf dune exec --profile=release bench/feel_bench.exe -- \
  -bench smt -only "Bin packing" -min-iterations 50 -max-iterations 50
memtrace-viewer bench/trace.ctf   # serves a flamegraph UI in the browser
```

Record notable results at the top of `bench/mac_smt_results.txt` /
`bench/results.txt` as before.
