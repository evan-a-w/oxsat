Always run ocamlformat after you finish (eg. ocamlformat */*.ml; ocamlformat */*.mli).

Prefer to write tests in expect test style (let%expect_test ..., prefer printing sexps to demonstrate state rather than eg. asserting things are equal to some manually defined value, look at the output of dune build @runtest, and if diffs are acceptable run dune promote to accept the changes)

Prefer to modularize code where possible. For instance, if there is functionality that is self contained, you should create a file module.ml with a [type t] defined inside, and expose only the minimal necessary functionality in the .mli file.

You can run the benchmarks, using the same commands as bench/results.txt. You
can add entries in a similar format. Be brief in descriptions of changes.

Eagerly remove dead code, unless you think it has a good chance of being useful
in future, and it doesn't add complexity / performance losses / prevent optimisations.
