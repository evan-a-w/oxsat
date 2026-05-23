open! Core
open! Import

type t =
  { clause : int Vec.Value.t
  ; mutable has_unit : bool
  }
