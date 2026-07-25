#!/usr/bin/env bash
# Summarize folded stacks into a flat profile.
#
# Input format (stdin or file argument), one stack per line:
#   frame;frame;leaf COUNT
#
# Output: table of self%/total% per frame, sorted by self time descending.
# "self" counts samples where the frame is the leaf; "total" counts samples
# where the frame appears anywhere in the stack (counted once per stack).
#
# Usage:
#   bench/folded_summary.sh bench/flamegraph.folded
#   some_profiler | bench/folded_summary.sh
set -eu
awk '
{
  n = split($0, fields, ";")
  last = fields[n]
  match(last, / [0-9]+$/)
  count = substr(last, RSTART + 1) + 0
  leaf = substr(last, 1, RSTART - 1)
  self[leaf] += count
  split("", seen)
  for (i = 1; i <= n; i++) {
    f = (i == n ? leaf : fields[i])
    if (!(f in seen)) { incl[f] += count; seen[f] = 1 }
  }
}
END {
  for (f in incl) printf "%d %d %s\n", self[f] + 0, incl[f], f
}
' "$@" \
  | sort -k1,1nr \
  | awk '
      {
        self[NR] = $1
        incl[NR] = $2
        match($0, /^[0-9]+ +[0-9]+ +/)
        name[NR] = substr($0, RLENGTH + 1)
        total += $1
        lines = NR
      }
      END {
        printf "%7s %7s %10s %10s  %s\n", "self%", "tot%", "self", "total", "function"
        for (i = 1; i <= lines; i++)
          printf "%6.2f%% %6.2f%% %10d %10d  %s\n", \
            100 * self[i] / total, 100 * incl[i] / total, self[i], incl[i], \
            name[i]
      }'
