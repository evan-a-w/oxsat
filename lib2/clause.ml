open! Core
open! Import

type t =
  { clause : int Vec.Value.t
  ; mutable has_unit : bool
  ; mutable learned : bool
  ; mutable lbd : int
  ; mutable deleted : bool
  ; mutable activity : float
  }
