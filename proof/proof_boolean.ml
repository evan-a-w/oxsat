open! Core
open! Theory_core

type t =
  | True
  | False
  | Atom of Proof_atom.t
  | Not of t
  | And of t list
  | Or of t list
[@@deriving sexp, compare]

let rec of_core : Boolean_formula.t -> t = function
  | True -> True
  | False -> False
  | Atom atom -> Atom (Proof_atom.Theory atom)
  | Not formula -> Not (of_core formula)
  | And formulas -> And (List.map formulas ~f:of_core)
  | Or formulas -> Or (List.map formulas ~f:of_core)
;;

let of_formula formula =
  Or_error.map (Boolean_formula.of_formula formula) ~f:of_core
;;
