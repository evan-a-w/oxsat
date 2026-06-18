open! Core
open! Feel.Import

type t =
  | Sat of { assignments : bool option array }
  | Unsat of { reason : Formula.t }
[@@deriving sexp]
