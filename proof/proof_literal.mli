open! Core

type t = private
  { atom : Proof_atom.t
  ; positive : bool
  }
[@@deriving sexp, compare]

val create : atom:Proof_atom.t -> positive:bool -> t
val neg : t -> t
