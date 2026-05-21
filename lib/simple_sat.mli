open! Core

(** A small SAT solver ported closely from [bench/sat.js]. It is intentionally
    separate from the main solver so it can serve as an apples-to-apples
    baseline in benchmarks. *)

val solve : size:int -> int array array -> bool

(** Parse a DIMACS string and solve it. If the DIMACS header does not specify
    the variable count, this falls back to the maximum variable seen in the
    clauses. *)
val solve_dimacs_string : string -> bool
