open! Core
open! Feel.Import

type t =
  | Sat of { assignments : bool option array }
  | Unsat of { core : Core_literal.t list }
[@@deriving sexp]
