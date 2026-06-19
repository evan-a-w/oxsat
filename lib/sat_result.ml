open! Core
open! Import

module Core_clause = struct
  type t =
    { literals : int array
    ; is_theory : bool
    }
  [@@deriving sexp]
end

type t =
  | Sat of { assignments : bool option array }
  | Unsat of { core : Core_clause.t list }
[@@deriving sexp]
