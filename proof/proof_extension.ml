open! Core

type t =
  { id : Proof_id.Extension.t
  ; definition : Proof_boolean.t
  }
[@@deriving sexp, compare]
