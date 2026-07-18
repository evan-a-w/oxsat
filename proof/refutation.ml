open! Core
open! Theory_core

module Input_clause = struct
  type t = { input : int } [@@deriving sexp, compare]
end

module Reason = struct
  type t =
    | Input_clause of Input_clause.t
    | Extension_definition of Proof_id.Extension.t
    | Theory_lemma of Proof_theory_certificate.t
    | Rup of { hints : Proof_id.Refutation_step.t array }
  [@@deriving sexp, compare]
end

module Step = struct
  type t =
    { clause : Proof_clause.t
    ; reason : Reason.t
    }
  [@@deriving sexp, compare]
end

type t =
  { inputs : Formula.any array
  ; extensions : Proof_extension.t array
  ; steps : Step.t array
  ; contradiction : Proof_id.Refutation_step.t
  }
[@@deriving sexp, compare]
