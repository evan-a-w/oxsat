Comparing two result files prints a table sorted by largest time regression
first, with time and allocation ratios (after/before) and a geomean row.

  $ cat > before.sexp <<EOF
  > (((name A) (stats ((mean 1000.) (stddev 0.) (p50 1000.) (p75 1000.) (p90 1000.) (p95 1000.) (p99 1000.) (samples 1) (alloc_bytes 100.) (major_collections 0.))))
  >  ((name B) (stats ((mean 2000.) (stddev 0.) (p50 2000.) (p75 2000.) (p90 2000.) (p95 2000.) (p99 2000.) (samples 1) (alloc_bytes 200.) (major_collections 0.)))))
  > EOF
  $ cat > after.sexp <<EOF
  > (((name A) (stats ((mean 2000.) (stddev 0.) (p50 2000.) (p75 2000.) (p90 2000.) (p95 2000.) (p99 2000.) (samples 1) (alloc_bytes 50.) (major_collections 0.))))
  >  ((name B) (stats ((mean 1000.) (stddev 0.) (p50 1000.) (p75 1000.) (p90 1000.) (p95 1000.) (p99 1000.) (samples 1) (alloc_bytes 400.) (major_collections 0.)))))
  > EOF
  $ ../feel_bench.exe before.sexp after.sexp
  name      before       after    time   alloc
  A       1.00 μs    2.00 μs   2.00x   0.50x
  B       2.00 μs    1.00 μs   0.50x   2.00x
  geomean                           1.00x   1.00x

In compare mode, -only filters both files by name:

  $ ../feel_bench.exe -only B before.sexp after.sexp
  name      before       after    time   alloc
  B       2.00 μs    1.00 μs   0.50x   2.00x
  geomean                           0.50x   2.00x

Benchmarks present in only one file are skipped with warnings:

  $ cat > x.sexp <<EOF
  > (((name X) (stats ((mean 1000.) (stddev 0.) (p50 1000.) (p75 1000.) (p90 1000.) (p95 1000.) (p99 1000.) (samples 1) (alloc_bytes 100.) (major_collections 0.)))))
  > EOF
  $ cat > y.sexp <<EOF
  > (((name Y) (stats ((mean 1000.) (stddev 0.) (p50 1000.) (p75 1000.) (p90 1000.) (p95 1000.) (p99 1000.) (samples 1) (alloc_bytes 100.) (major_collections 0.)))))
  > EOF
  $ ../feel_bench.exe x.sexp y.sexp 2>err.txt
  No benchmarks in common.
  $ cat err.txt
  Warning: Y not present in before file, skipping
  Warning: X not present in after file, skipping

A results file saved with -o round-trips: comparing a file against itself
gives all 1.00x ratios. (Only the ratio columns are checked, since times vary
between runs.)

  $ ../feel_bench.exe -bench smt -only "EUF web (n=20" -min-iterations 1 -max-iterations 1 -sample-runs 1 -o out.sexp > /dev/null
  $ ../feel_bench.exe out.sexp out.sexp | awk '{print $(NF-1), $NF}'
  time alloc
  1.00x 1.00x
  1.00x 1.00x
