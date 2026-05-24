open! Core
open! Import

type t =
  { mutable assignment : bool or_null
  ; mutable trail_entry : Trail_entry.Option_u.t
  ; watched_clauses : Watched_clause.Vec.t Tf_pair.t
  }
