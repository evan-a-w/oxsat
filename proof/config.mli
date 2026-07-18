open! Core

(* TODO: use in [entails] *)
type t = { use_sat_solver_in_checks : bool } [@@deriving sexp]
