open! Core
open! Import

type t =
  | Sat of { assignments : bool or_null array }
  | Unsat of { global_ unsat_core : int array }
[@@deriving sexp]
