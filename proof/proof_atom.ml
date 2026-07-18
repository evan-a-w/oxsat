open! Core
open! Theory_core

type t =
  | Theory of Atom.t
  | Extension of Proof_id.Extension.t
[@@deriving sexp, compare]

let normalize = function
  | Theory atom -> Theory (Atom.normalize atom)
  | Extension _ as atom -> atom
;;
