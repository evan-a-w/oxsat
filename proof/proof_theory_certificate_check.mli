open! Core
open! Theory_core

val check
  :  ?datatype_env:Datatype.Env.t
  -> clause:Proof_clause.t
  -> Proof_theory_certificate.t
  -> unit Or_error.t
