open! Core
open! Theory_core

(** A solver-independent clause refutation. Clauses refer to semantic theory
    atoms and proof-local extension variables, never the SAT solver's integer
    variables or mutable clause indices. *)

module Input_clause : sig
  type t =
    { input : int
    ; literal : Proof_literal.t
    }
  [@@deriving sexp, compare]
end

module Reason : sig
  type t =
    | Input_clause of Input_clause.t
    | Extension_definition of Proof_id.Extension.t
    | Theory_lemma of Proof_theory_certificate.t
    | Rup of { hints : Proof_id.Refutation_step.t array }
  [@@deriving sexp, compare]
end

(** A [Rup] step is justified by reverse unit propagation over the cited earlier
    clauses. *)

module Step : sig
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

val check : t -> unit Or_error.t

(** [contradiction] must identify a step whose conclusion is the empty clause.
    Inputs and extension definitions are retained so a checker can validate
    input CNF independently of the encoding used by the solver. *)
