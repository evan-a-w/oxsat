open! Core

(** A small SAT solver ported closely from [bench/sat.js]. It is intentionally
    separate from the main solver so it can serve as an apples-to-apples
    baseline in benchmarks. *)

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
      { add_clause : Bucket.t
      ; select_literal : Bucket.t
      ; unit_propagate : Bucket.t
      ; backtrack : Bucket.t
      }
    [@@deriving sexp]
  end

  type t =
    { decisions : int
    ; conflicts : int
    ; learned : int
    ; learned_clause_literals : int
    ; assignments : int
    ; profile : Profile.t option
    }
  [@@deriving sexp]
end

val solve : ?profile:bool -> size:int -> int array array -> bool

val solve_with_stats
  :  ?profile:bool
  -> size:int
  -> int array array
  -> bool * Stats.t

(** Parse a DIMACS string and solve it. If the DIMACS header does not specify
    the variable count, this falls back to the maximum variable seen in the
    clauses. *)
val solve_dimacs_string : ?profile:bool -> string -> bool

val solve_dimacs_string_with_stats : ?profile:bool -> string -> bool * Stats.t
