#!/usr/bin/env bash
# CPU-profile a benchmark and view the results.
#
# Usage:
#   bench/profile.sh [--profiler flamegraph|samply|perf] [--text] [profiler opts] -- [feel_bench args]
#
# Examples:
#   bench/profile.sh -- -bench smt -only "Bin packing (items=15"
#   bench/profile.sh --profiler samply -- -bench smt -only "Bin packing"
#   bench/profile.sh -o bench/binpacking.svg -- -bench smt -only "Bin packing"
#   bench/profile.sh --text -- -bench smt -only "Bin packing"
#
# --text (flamegraph only) also writes the folded stacks to
# bench/flamegraph.folded and prints a textual flat profile (saved to
# bench/flamegraph.txt), suitable for reading without a browser.
#
# Profilers (both cross-platform: Linux + macOS, x86 + arm):
#   flamegraph  cargo install flamegraph. Writes an interactive svg (default
#               bench/flamegraph.svg); open it in a browser yourself. Uses perf
#               on Linux and xctrace (Time Profiler) on macOS; no root needed.
#   samply      cargo install samply. Records a profile (default
#               bench/samply.json.gz) and opens the Firefox Profiler UI in
#               your browser automatically. No root needed. Re-open later
#               with: samply load bench/samply.json.gz
#   perf        Linux only, no cargo needed. Records perf.data (default
#               bench/perf.data) and with --text writes folded stacks plus a
#               flat profile. Useful where flamegraph/samply aren't installed
#               (e.g. WSL2). Set PERF=/path/to/perf if the perf on PATH does
#               not match your kernel, as happens on WSL2:
#                 PERF=/usr/lib/linux-tools-5.15.0-186/perf \
#                   bench/profile.sh --profiler perf --text -- -bench smt
#
# Defaults to a fixed number of iterations so the profile reflects the
# benchmark rather than harness calibration (override by passing your own
# -min-iterations/-max-iterations/-sample-runs after --).
set -eu
cd "$(dirname "$0")/.."

profiler="flamegraph"
text=0
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
  elif [ "$seen_sep" = 0 ] && [ "$arg" = "--text" ]; then
    text=1
  elif [ "$seen_sep" = 0 ] && [ "$arg" = "--" ]; then
    seen_sep=1
  elif [ "$seen_sep" = 1 ]; then
    bench_args+=("$arg")
  else
    profiler_args+=("$arg")
  fi
done
if [ "$text" = 1 ] && [ "$profiler" != flamegraph ] && [ "$profiler" != perf ]; then
  echo "error: --text is only supported with --profiler flamegraph or perf" >&2
  exit 1
fi
if [ "$expect_profiler" = 1 ]; then
  echo "error: --profiler expects an argument (flamegraph, samply or perf)" >&2
  exit 1
fi
case "$profiler" in
  flamegraph | samply | perf) ;;
  *)
    echo "error: unknown profiler '$profiler' (expected flamegraph, samply or perf)" >&2
    exit 1
    ;;
esac

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
    perf) out="bench/perf.data" ;;
  esac
  [ "$profiler" = perf ] || profiler_args+=(-o "$out")
fi

dune build --profile=release bench/feel_bench.exe 2>/dev/null \
  || { eval "$(opam env)" && dune build --profile=release bench/feel_bench.exe; }
exe="$PWD/_build/default/bench/feel_bench.exe"

case "$profiler" in
  flamegraph)
    text_args=()
    folded_file="bench/flamegraph.folded"
    [ "$text" = 1 ] && text_args=(--post-process "tee $folded_file")
    flamegraph "${profiler_args[@]+"${profiler_args[@]}"}" \
      "${text_args[@]+"${text_args[@]}"}" -- "$exe" "${bench_args[@]}"
    echo "Wrote $out (open in a browser)"
    if [ "$text" = 1 ]; then
      bench/folded_summary.sh "$folded_file" | tee bench/flamegraph.txt
    fi
    ;;
  samply)
    samply record "${profiler_args[@]+"${profiler_args[@]}"}" \
      -- "$exe" "${bench_args[@]}"
    echo "Wrote $out (re-open with: samply load $out)"
    ;;
  perf)
    perf_bin="${PERF:-perf}"
    "$perf_bin" record -F 999 -g --call-graph dwarf -o "$out" \
      "${profiler_args[@]+"${profiler_args[@]}"}" -- "$exe" "${bench_args[@]}"
    echo "Wrote $out (inspect with: $perf_bin report -i $out)"
    if [ "$text" = 1 ]; then
      folded_file="bench/flamegraph.folded"
      "$perf_bin" script -i "$out" \
        | bench/perf_fold.sh >"$folded_file"
      bench/folded_summary.sh "$folded_file" | tee bench/flamegraph.txt
    fi
    ;;
esac
