open! Core
open! Feel.Import

(** A single literal in an unsat core with its theory atom resolved. *)
type t =
  | Pos of Atom.t (** atom asserted true *)
  | Neg of Atom.t (** atom asserted false *)
  | Raw of int
  (** internal variable with no user-visible atom (e.g. Tseitin auxiliary) *)
[@@deriving sexp]
