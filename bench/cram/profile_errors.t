profile.sh rejects invalid flag combinations before doing any work:

  $ bash ../profile.sh --text --profiler samply -- -bench smt
  error: --text is only supported with --profiler flamegraph
  [1]

  $ bash ../profile.sh --profiler bogus -- -bench smt
  error: unknown profiler 'bogus' (expected flamegraph or samply)
  [1]

  $ bash ../profile.sh --profiler
  error: --profiler expects an argument (flamegraph or samply)
  [1]
