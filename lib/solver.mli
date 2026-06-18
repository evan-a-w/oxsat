open! Core
open! Import

type t

type time_bound =
  [ `Unlimited
  | `Bounded of int
  ]

exception Timeout

val%template solve
  :  ?time_bound:time_bound
  -> ?assumptions:int array @ local
  -> t
  -> Sat_result.t @ m
[@@alloc a @ m = (heap_global, stack_local)]

val create
  :  ?theory:Theory.Packed.t
  -> ?random_state:Random.State.t
  -> ?debug:bool @ local
  -> unit
  -> t

val create_with_formula
  :  ?theory:Theory.Packed.t
  -> ?debug:bool @ local
  -> int array array
  -> [ `Ok of t | `Unsat of Sat_result.Proof_clause.t list ]

val add_clause
  :  t
  -> clause:int array
  -> [ `Ok | `Unsat of Sat_result.Proof_clause.t list ]

val stats : t -> Stats.t
