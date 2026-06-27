open! Core
open! Import

module Origin = struct
  type t =
    | User
    | Theory
    | Learned
end

type t =
  { clause : int Vec.Value.t
  ; mutable has_unit : bool
  ; mutable origin : Origin.t
  ; mutable lbd : int
  ; mutable deleted : bool
  ; mutable activity : float
  }
