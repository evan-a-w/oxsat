open! Core

type t =
  | Theory of Atom.t
  | Extension of Proof_id.Extension.t
[@@deriving sexp, compare]

val normalize : t -> t
