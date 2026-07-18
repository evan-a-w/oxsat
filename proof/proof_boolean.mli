open! Core

(** Boolean expressions over semantic theory atoms and proof-local extension
    variables. This is deliberately separate from {!Formula.any}: extension
    variables have no meaning outside a proof certificate. *)

type t =
  | True
  | False
  | Atom of Proof_atom.t
  | Not of t
  | And of t list
  | Or of t list
[@@deriving sexp, compare]
