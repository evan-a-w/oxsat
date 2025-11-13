open! Core
open! Import

type t : value & value
type clause = t

val copy : t -> t
val is_satisfied : t -> assignments:Bitset.t Tf_pair.t -> bool
val clear : t -> unit
val iter_literals : t -> f:(Literal.t -> unit) @ local -> unit

val%template literals_list : t @ local -> int list @ m
[@@alloc a @ m = (stack_local, heap_global)]

val contains : t -> var:int -> bool
val contains_literal : t -> literal:Literal.t -> bool
val value_exn : t -> var:int -> bool
val can_resolve : t -> other:t -> on_var:int -> bool
val resolve_exn : t -> other:t -> on_var:int -> unit
val of_int_array : int array -> t
val to_int_array : t -> int array
val unit_literal : t -> assignments:Bitset.t Tf_pair.t -> Literal.Option.t

module Or_trivial : sig
  type (_ : value & value) tag =
    | Trivial : _ tag
    | Non_trivial : t tag

  type t : immediate & (value & value) = T : #('a tag * 'a) -> t [@@unboxed]

  val trivial : unit -> t
  val non_trivial : clause -> t

  module Vec : Vec.S [@kind value & (value & value)] with type Elt.t := t [@@ocamlformat "disable"]
end

module Option : sig
  type value = t
  type t = Or_trivial.t

  module Vec = Or_trivial.Vec

  val none : unit -> t [@@zero_alloc]
  val some : value -> t [@@zero_alloc]
  val is_none : t -> bool [@@zero_alloc]
  val is_some : t -> bool [@@zero_alloc]
  val value : t -> default:value -> value [@@zero_alloc]
  val value_exn : t -> value [@@zero_alloc]

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool [@@zero_alloc]
      val unsafe_value : t -> value [@@zero_alloc]
    end
  end
end

module Vec : Vec.S [@kind value & value] with type Elt.t := t
module Pool : Pool.S [@kind value & value] with type Elt.t := t
