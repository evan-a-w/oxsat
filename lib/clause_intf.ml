open! Core

module type S = sig
  type t [@@deriving sexp]

  val copy : t -> t
  val is_satisfied : t -> assignments:Ds.Bitset.t Tf_pair.t -> bool
  val clear : t -> unit
  val iter_literals : t -> f:(Literal.t -> unit) -> unit
  val is_tautology : t -> bool
  val literals_list : t -> int list
  val contains : t -> var:int -> bool
  val contains_literal : t -> literal:Literal.t -> bool
  val can_resolve : t -> other:t -> on_var:int -> bool
  val resolve_exn : t -> other:t -> on_var:int -> unit
  val of_int_array : int array -> t
  val to_int_array : t -> int array
  val unit_literal : t -> assignments:Ds.Bitset.t Tf_pair.t -> Literal.Option.t

  module Pool : Ds.Pool_intf.S with type Elt.t := t
end
