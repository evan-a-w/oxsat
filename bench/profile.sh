#!/usr/bin/env bash
# CPU-profile a benchmark and view the results.
#
# Usage:
#   bench/profile.sh [--profiler flamegraph|samply] [profiler opts] -- [feel_bench args]
#
# Examples:
#   bench/profile.sh -- -bench smt -only "Bin packing (items=15"
#   bench/profile.sh --profiler samply -- -bench smt -only "Bin packing"
#   bench/profile.sh -o bench/binpacking.svg -- -bench smt -only "Bin packing"
#
# Profilers (both cross-platform: Linux + macOS, x86 + arm):
#   flamegraph  cargo install flamegraph. Writes an interactive svg (default
#               bench/flamegraph.svg); open it in a browser yourself. Uses perf
#               on Linux; on macOS uses dtrace, which needs root, so the
#               workload is re-run under sudo via flamegraph's --root.
#   samply      cargo install samply. Records a profile (default
#               bench/samply.json.gz) and opens the Firefox Profiler UI in
#               your browser automatically. No root needed. Re-open later
#               with: samply load bench/samply.json.gz
#
# Defaults to a fixed number of iterations so the profile reflects the
# benchmark rather than harness calibration (override by passing your own
# -min-iterations/-max-iterations/-sample-runs after --).
set -eu
cd "$(dirname "$0")/.."

profiler="flamegraph"
profiler_args=()
bench_args=()
seen_sep=0
expect_profiler=0
for arg in "$@"; do
  if [ "$expect_profiler" = 1 ]; then
    profiler="$arg"
    expect_profiler=0
  elif [ "$seen_sep" = 0 ] && [ "$arg" = "--profiler" ]; then
    expect_profiler=1
  elif [ "$seen_sep" = 0 ] && [ "$arg" = "--" ]; then
    seen_sep=1
  elif [ "$seen_sep" = 1 ]; then
    bench_args+=("$arg")
  else
    profiler_args+=("$arg")
  fi
done
if [ "$expect_profiler" = 1 ]; then
  echo "error: --profiler expects an argument (flamegraph or samply)" >&2
  exit 1
fi

has_flag() {
  local flag="$1"
  shift
  for arg in "$@"; do [ "$arg" = "$flag" ] && return 0; done
  return 1
}

has_flag -min-iterations "${bench_args[@]+"${bench_args[@]}"}" \
  || bench_args+=(-min-iterations 50)
has_flag -max-iterations "${bench_args[@]+"${bench_args[@]}"}" \
  || bench_args+=(-max-iterations 50)
has_flag -sample-runs "${bench_args[@]+"${bench_args[@]}"}" \
  || bench_args+=(-sample-runs 5)

out=""
if has_flag -o "${profiler_args[@]+"${profiler_args[@]}"}"; then
  prev=""
  for arg in "${profiler_args[@]+"${profiler_args[@]}"}"; do
    [ "$prev" = "-o" ] && out="$arg"
    prev="$arg"
  done
else
  case "$profiler" in
    flamegraph) out="bench/flamegraph.svg" ;;
    samply) out="bench/samply.json.gz" ;;
  esac
  profiler_args+=(-o "$out")
fi

dune build --profile=release bench/feel_bench.exe 2>/dev/null \
  || { eval "$(opam env)" && dune build --profile=release bench/feel_bench.exe; }
exe="$PWD/_build/default/bench/feel_bench.exe"

case "$profiler" in
  flamegraph)
    if [ "$(uname)" = "Darwin" ]; then
      flamegraph --root "${profiler_args[@]+"${profiler_args[@]}"}" \
        -- "$exe" "${bench_args[@]}"
    else
      flamegraph "${profiler_args[@]+"${profiler_args[@]}"}" \
        -- "$exe" "${bench_args[@]}"
    fi
    echo "Wrote $out (open in a browser)"
    ;;
  samply)
    samply record "${profiler_args[@]+"${profiler_args[@]}"}" \
      -- "$exe" "${bench_args[@]}"
    echo "Wrote $out (re-open with: samply load $out)"
    ;;
  *)
    echo "error: unknown profiler '$profiler' (expected flamegraph or samply)" >&2
    exit 1
    ;;
esac
