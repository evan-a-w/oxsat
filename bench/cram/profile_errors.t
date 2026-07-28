profile.sh rejects invalid flag combinations before doing any work:

  $ bash ../profile.sh --text --profiler samply -- -bench smt
  error: --text is only supported with --profiler flamegraph or perf
  [1]

  $ bash ../profile.sh --profiler bogus -- -bench smt
  error: unknown profiler 'bogus' (expected flamegraph, samply or perf)
  [1]

  $ bash ../profile.sh --profiler
  error: --profiler expects an argument (flamegraph, samply or perf)
  [1]
