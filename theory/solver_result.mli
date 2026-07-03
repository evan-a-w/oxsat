open! Core
open! Feel.Import

module Core_step : sig
  type t =
    | Asserted of Formula.t
    | Theory_lemma of Formula.t
  [@@deriving sexp]
end

type t =
  | Sat of { tvar_assignments : Tvar_assignment.t Tvar.Map.t }
  | Unsat of { core : Core_step.t list }
[@@deriving sexp]
