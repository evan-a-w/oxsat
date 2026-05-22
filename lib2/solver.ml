open! Core
open! Import

type t =
  { trail : int Vec.Value.t
  ; mutable trail_processed_till : int
  ; mutable has_empty_clause : bool
  ; vars : Var.Vec.t
  }
