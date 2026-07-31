## Reproducing the OCaml/OxCaml environment

This project is built with OxCaml, using the following opam repositories:

- ox: git+https://github.com/oxcaml/opam-repository.git
- default: https://opam.ocaml.org

ADT note: algebraic datatypes are checked against explicit datatype declarations.
Model checking for ADTs is finite-ground over the terms observed by the solver's
lazy ADT lemmas; it is not a full non-ground ADT model representation.

Create a local switch:

```sh
opam switch create . --repos ox=git+https://github.com/oxcaml/opam-repository.git,default=https://opam.ocaml.org ocaml-variants.5.2.0+ox && eval "$(opam env)" && opam install . -y --locked --deps-only && dune build
