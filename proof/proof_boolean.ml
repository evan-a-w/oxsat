open! Core

type t =
  | True
  | False
  | Atom of Proof_atom.t
  | Not of t
  | And of t list
  | Or of t list
[@@deriving sexp, compare]
