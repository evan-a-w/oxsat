open! Core

val check
  :  clause:Proof_clause.t
  -> Proof_theory_certificate.t
  -> unit Or_error.t
