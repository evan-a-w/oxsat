open! Core

type t =
  { atom : Proof_atom.t
  ; positive : bool
  }
[@@deriving sexp, compare]

let create ~atom ~positive = { atom = Proof_atom.normalize atom; positive }
let neg t = { t with positive = not t.positive }
