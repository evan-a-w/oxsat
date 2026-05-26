open! Core
open! Import

type t

val%template solve : ?assumptions:int array @ local -> t -> Sat_result.t @ m
[@@alloc a @ m = (heap_global, stack_local)]

val create : ?debug:bool @ local -> ?timeout:Time_ns.Span.t -> unit -> t

val create_with_formula
  :  ?debug:bool @ local
  -> ?timeout:Time_ns.Span.t
  -> int array array
  -> t

val add_clause : t -> clause:int array -> unit
val stats : t -> Stats.t
