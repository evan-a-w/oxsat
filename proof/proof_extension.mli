open! Core

(** The Boolean definition of a proof-local variable introduced by
    clausification. Definitions may refer only to earlier extension IDs once
    checked. *)

type t =
  { id : Proof_id.Extension.t
  ; definition : Proof_boolean.t
  }
[@@deriving sexp, compare]
