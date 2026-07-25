#!/usr/bin/env bash
# Run a benchmark suite and compare against its checked-in baseline in
# bench/comparisons/.
#
# Usage:
#   bench/compare.sh smt|sat|dimacs [extra feel_bench flags]
#
# Examples:
#   bench/compare.sh smt
#   bench/compare.sh smt -only "Bin packing"
#
# To update a baseline after an intentional change, re-run the suite's
# canonical command with -o:
#   dune exec --profile=release bench/feel_bench.exe -- -bench smt \
#     -o bench/comparisons/smt.sexp
#   dune exec --profile=release bench/feel_bench.exe -- -bench sat -sat-max-n 80 \
#     -o bench/comparisons/sat.sexp
#   dune exec --profile=release bench/feel_bench.exe -- -bench dimacs \
#     -min-iterations 1 -max-iterations 1 -sample-runs 1 \
#     -o bench/comparisons/dimacs.sexp
set -eu
cd "$(dirname "$0")/.."

if [ $# -lt 1 ]; then
  echo "usage: bench/compare.sh smt|sat|dimacs [extra feel_bench flags]" >&2
  exit 1
fi
suite="$1"
shift
case "$suite" in
  smt) args=(-bench smt) ;;
  sat) args=(-bench sat -sat-max-n 80) ;;
  dimacs) args=(-bench dimacs -min-iterations 1 -max-iterations 1 -sample-runs 1) ;;
  *)
    echo "error: unknown suite '$suite' (expected smt, sat or dimacs)" >&2
    exit 1
    ;;
esac

baseline="bench/comparisons/$suite.sexp"
if [ ! -f "$baseline" ]; then
  echo "error: no baseline at $baseline (see header of $0 for how to create it)" >&2
  exit 1
fi

dune build --profile=release bench/feel_bench.exe 2>/dev/null \
  || { eval "$(opam env)" && dune build --profile=release bench/feel_bench.exe; }
exe="$PWD/_build/default/bench/feel_bench.exe"

after="$(mktemp /tmp/oxsat-after.XXXXXX.sexp)"
trap 'rm -f "$after"' EXIT
"$exe" "${args[@]}" "$@" -o "$after"
echo ""
echo "=== Comparison vs $baseline ==="
"$exe" "$@" "$baseline" "$after"
