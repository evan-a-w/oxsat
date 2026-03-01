open! Core

module Sat_result : sig
  type t =
    | Sat of { assignments : Clause.t }
    | Unsat of { unsat_core : Clause.t }
  [@@deriving sexp]
end

type t

val solve : ?assumptions:int array -> t -> Sat_result.t
val create : ?debug:bool -> unit -> t
val create_with_formula : ?debug:bool -> int array array -> t

val add_clause : t -> clause:Clause.t -> t
val add_clause' : t -> clause:int array -> t
