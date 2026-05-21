open! Core
open! Import

module Sat_result : sig
  type t =
    | Sat of { assignments : Clause.t }
    | Unsat of { global_ unsat_core : Clause.t }
  [@@deriving sexp]
end

type t

module Stats : sig
  module Profile : sig
    module Bucket : sig
      type t =
        { count : int
        ; elapsed_ns : float
        }
      [@@deriving sexp]
    end

    type t =
      { update_watched_clauses : Bucket.t
      ; update_watched_clauses_stale_cleanup : Bucket.t
      ; update_watched_clauses_analyze_false_watch : Bucket.t
      ; update_watched_clauses_move_watch : Bucket.t
      ; update_watched_clauses_queue_unit : Bucket.t
      ; update_watched_clauses_conflict : Bucket.t
      ; restart : Bucket.t
      ; restart_clause_rescan : Bucket.t
      ; analyze_conflict : Bucket.t
      ; simplify_clauses : Bucket.t
      ; simplify_learned_clause : Bucket.t
      ; clause_lbd : Bucket.t
      }
    [@@deriving sexp]
  end

  type t =
    { iterations : int
    ; decisions : int
    ; propagations : int
    ; conflicts : int
    ; learned_clauses : int
    ; learned_clause_literals : int
    ; max_decision_level : int
    ; profile : Profile.t option
    }
  [@@deriving sexp]
end

val%template solve : ?assumptions:int array @ local -> t -> Sat_result.t @ m
[@@alloc a @ m = (heap_global, stack_local)]

val create : ?debug:bool @ local -> ?profile:bool @ local -> unit -> t

val create_with_formula
  :  ?debug:bool @ local
  -> ?profile:bool @ local
  -> int array array
  -> t

(** mutate [t], but just return it for convenience sake *)
val add_clause : t -> clause:Clause.t -> t

val add_clause' : t -> clause:int array -> t
val stats : t -> Stats.t
