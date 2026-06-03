(lang dune 3.20)

(name feel)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name feel)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune core)
 (tags
  ("add topics" "to describe" your project)))

(package
 (name ds)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune core)
 (tags
  ("add topics" "to describe" your project)))

(package
 (name theories)
 (synopsis "SAT theory implementations for DPLL(T)")
 (description "Theory plugins for use with the Feel DPLL(T) solver")
 (depends ocaml dune core feel)
 (tags
  ("sat" "dpll" "theories")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
