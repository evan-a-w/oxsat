open! Core

(** A well-formed Boolean formula after elaborating overloaded equality and
    arithmetic comparisons into normalized theory atoms. *)
type t =
  | True
  | False
  | Atom of Atom.t
  | Not of t
  | And of t list
  | Or of t list
[@@deriving sexp, compare]

val of_formula : Formula.any -> t Or_error.t
