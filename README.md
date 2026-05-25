## Reproducing the OCaml/OxCaml environment

This project is built with OxCaml, using the following opam repositories:

- ox: git+https://github.com/oxcaml/opam-repository.git
- default: https://opam.ocaml.org

Create a local switch:

```sh
opam switch create . --repos ox=git+https://github.com/oxcaml/opam-repository.git,default=https://opam.ocaml.org ocaml-variants.5.2.0+ox && eval "$(opam env)" && opam install . -y --locked --deps-only && dune build
