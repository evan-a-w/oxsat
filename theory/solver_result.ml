open! Core
open! Feel.Import

module Core_step = struct
  type t =
    | Asserted of Formula.any
    | Theory_lemma of Formula.any
  [@@deriving sexp_of]
end

type t =
  | Sat of { tvar_assignments : Tvar_assignment.t Tvar.Map.t }
  | Unsat of { core : Core_step.t list }
[@@deriving sexp_of]
