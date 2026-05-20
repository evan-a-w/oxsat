open! Core
open! Import

type t [@@deriving sexp]

val copy : t -> t
val length : t -> int
val get : t -> int -> int
val set : t -> int -> int -> unit
val iteri : t -> f:(int -> Literal.t -> unit) @ local -> unit
val is_satisfied : t -> assignments:Bitset.t Tf_pair.t -> bool
val clear : t -> unit
val iter_literals : t -> f:(Literal.t -> unit) @ local -> unit
val is_tautology : t -> bool

val%template literals_list : t -> int list @ m
[@@alloc a @ m = (stack_local, heap_global)]

val contains : t -> var:int -> bool
val contains_literal : t -> literal:Literal.t -> bool
val can_resolve : t -> other:t -> on_var:int -> bool
val resolve_exn : t -> other:t -> on_var:int -> unit
val of_int_array : ?lbd:int -> ?learnt:bool -> int array -> t
val to_int_array : t -> int array
val unit_literal : t -> assignments:Bitset.t Tf_pair.t -> Literal.Option.t

type watched_clause_update =
  | Satisfied
  | Replacement of int
  | Unit of Literal.t
  | Conflict

val analyze_false_watch
  :  t
  -> assignments:Bitset.t Tf_pair.t
  -> false_watch_pos:int
  -> other_watch_pos:int
  -> watched_clause_update

val activity : t -> F64.t
val set_activity : t -> F64.t -> unit
val lbd : t -> int
val set_lbd : t -> int -> unit
val learnt : t -> bool
val set_learnt : t -> bool -> unit
val deleted : t -> bool
val set_deleted : t -> bool -> unit
val generation : t -> int
val bump_generation : t -> int
val pending_unit_generation : t -> int
val set_pending_unit_generation : t -> int -> unit
val watch_pos : t -> watch:int -> int
val set_watch_pos : t -> watch:int -> int -> unit
val watch_slot : t -> watch:int -> int
val set_watch_slot : t -> watch:int -> int -> unit
val clear_watch_data : t -> unit

module Pool : Pool_intf.S with type Elt.t := t

val negate : t -> unit
