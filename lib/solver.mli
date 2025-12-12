open! Core
open! Import

module Sat_result : sig
  type t =
    | Sat of { assignments : Clause.t }
    | Unsat of { global_ unsat_core : Clause.t }
  [@@deriving sexp]
end

type t

val%template solve : ?assumptions:int array @ local -> t -> Sat_result.t @ m
[@@alloc a @ m = (heap_global, stack_local)]

val create : ?debug:bool @ local -> unit -> t
val create_with_formula : ?debug:bool @ local -> int array array -> t

(** mutate [t], but just return it for convenience sake *)
val add_clause : t -> clause:Clause.t -> t

val add_clause' : t -> clause:int array -> t
