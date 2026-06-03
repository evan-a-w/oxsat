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
  :  ?random_state:Random.State.t
  -> ?debug:bool @ local
  -> ?theory:Theory.Packed.t
  -> unit
  -> t

val create_with_formula
  :  ?debug:bool @ local
  -> int array array
  -> [ `Ok of t | `Unsat of int array ]

val add_clause : t -> clause:int array -> [ `Ok | `Unsat of int array ]
val stats : t -> Stats.t
