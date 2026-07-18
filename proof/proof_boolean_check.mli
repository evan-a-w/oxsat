open! Core

val of_literal : Proof_literal.t -> Proof_boolean.t
val of_clause : Proof_clause.t -> Proof_boolean.t

(** Propositional entailment, treating each proof atom as an independent Boolean
    variable. Intended for checking local proof steps. *)
val entails
  :  assumptions:Proof_boolean.t list
  -> conclusion:Proof_boolean.t
  -> bool

val equivalent : Proof_boolean.t -> Proof_boolean.t -> bool

val expand_extensions
  :  extensions:Proof_extension.t array
  -> Proof_boolean.t
  -> Proof_boolean.t Or_error.t
