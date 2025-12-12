open! Core
open! Import

module type%template S = sig
  type t : k [@@deriving sexp]

  val copy : t -> t
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
  val of_int_array : int array -> t
  val to_int_array : t -> int array
  val unit_literal : t -> assignments:Bitset.t Tf_pair.t -> Literal.Option.t

  module Pool : Pool_intf.S [@kind k] with type Elt.t := t
end
[@@kind k = (value, value & value)]
