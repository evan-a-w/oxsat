open! Core
open! Feel.Import

module Core_step : sig
  type t =
    | Asserted of Formula.t
    | Tautology of Formula.t
  [@@deriving sexp]
end

type t =
  | Sat of { assignments : bool option array }
  | Unsat of { core : Core_step.t list }
[@@deriving sexp]
