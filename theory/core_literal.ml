open! Core
open! Feel.Import

type t =
  | Pos of Atom.t
  | Neg of Atom.t
  | Raw of int
[@@deriving sexp]
