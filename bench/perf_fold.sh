#!/usr/bin/env bash
# Collapse `perf script` output into folded stacks (stackcollapse-perf style).
#
# Input (stdin or file argument) is the output of `perf script`; output is one
# line per unique stack, "root;...;leaf COUNT", suitable for
# bench/folded_summary.sh or flamegraph.pl.
#
# Usage:
#   perf script -i bench/perf.data | bench/perf_fold.sh > bench/flamegraph.folded
set -eu
awk '
function flush() {
  if (depth > 0) {
    stack = frame[depth]
    for (i = depth - 1; i >= 1; i--) stack = stack ";" frame[i]
    counts[stack]++
  }
  depth = 0
}
# Blank line terminates a sample.
/^$/ { flush(); next }
# Header line for a sample (e.g. "exe 1234 12.34: cycles:u:"); starts a stack.
/^[^ \t]/ { flush(); next }
{
  # Stack frames look like "    ffffffff symbol+0x0 (/path/to/obj)".
  line = $0
  sub(/^[ \t]+/, "", line)
  sub(/^[0-9a-fA-F]+ /, "", line)
  sub(/ \([^)]*\)$/, "", line)
  sub(/\+0x[0-9a-fA-F]+$/, "", line)
  if (line == "" || line == "[unknown]") line = "[unknown]"
  gsub(/;/, ":", line)
  depth++
  frame[depth] = line
}
END {
  flush()
  for (s in counts) printf "%s %d\n", s, counts[s]
}
' "$@"
