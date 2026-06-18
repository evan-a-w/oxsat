open! Core
open! Import

module Proof_clause = struct
  type t =
    { literals : int array
    ; is_theory : bool
    }
  [@@deriving sexp]
end

type t =
  | Sat of { assignments : bool option array }
  | Unsat of { proof : Proof_clause.t list }
[@@deriving sexp]
