open! Core

module type Elt = sig
  type t

  val create_for_vec : unit -> t
end

module type S = sig
  module Elt : Elt

  type t

  val create : ?capacity:int -> unit -> t
  val length : t -> int
  val get : t -> int -> Elt.t
  val set : t -> int -> Elt.t -> unit
  val iter : t -> f:(Elt.t -> unit) -> unit
  val iteri : t -> f:(int -> Elt.t -> unit) -> unit
  val iteri_rev : t -> f:(int -> Elt.t -> unit) -> unit
  val iter_rev : t -> f:(Elt.t -> unit) -> unit
  val fold : t -> init:'b -> f:('b -> Elt.t -> 'b) -> 'b
  val foldr : t -> init:'b -> f:('b -> Elt.t -> 'b) -> 'b
  val push : t -> Elt.t -> unit
  val pop_exn : t -> Elt.t
  val fill_to_length : t -> length:int -> f:(int -> Elt.t) -> unit
  val take : t -> other:t -> unit
  val switch : t -> t -> unit
  val last_exn : t -> Elt.t
  val filter : t -> f:(Elt.t -> bool) -> t
  val filter_inplace : t -> f:(Elt.t -> bool) -> unit
  val map_inplace : t -> f:(Elt.t -> Elt.t) -> unit
  val singleton : Elt.t -> t
  val append : t -> t -> unit
  val clear : t -> unit
  val reverse_inplace : t -> unit
end

module type S_value = sig
  type 'a t

  val create : ?capacity:int -> unit -> 'a t
  val length : 'a t -> int
  val get : 'a t -> int -> 'a
  val get_opt : 'a t -> int -> 'a option
  val set : 'a t -> int -> 'a -> unit
  val iter : 'a t -> f:('a -> unit) -> unit
  val iter_nested : 'a t t -> f:('a -> unit) -> unit
  val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
  val iteri_rev : 'a t -> f:(int -> 'a -> unit) -> unit
  val iter_rev : 'a t -> f:('a -> unit) -> unit
  val copy : 'a t -> 'a t
  val of_array_taking_ownership : 'a array -> 'a t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val foldr : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val push : 'a t -> 'a -> unit
  val pop_exn : 'a t -> 'a
  val fill_to_length : 'a t -> length:int -> f:(int -> 'a) -> unit
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val sort : 'a t -> compare:('a -> 'a -> int) -> unit
  val sort_partitioned : 'a t -> a_len:int -> compare:('a -> 'a -> int) -> unit
  val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'b t
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val to_array : 'a t -> 'a array
  val mem : 'a t -> 'a -> compare:('a -> 'a -> int) -> bool
  val take : 'a t -> other:'a t -> unit
  val switch : 'a t -> 'a t -> unit
  val last : 'a t -> 'a option
  val last_exn : 'a t -> 'a
  val filter : 'a t -> f:('a -> bool) -> 'a t
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
  val filter_inplace : 'a t -> f:('a -> bool) -> unit
  val filter_map_inplace : 'a t -> f:('a -> 'a option) -> unit
  val findi : 'a t -> f:(int -> 'a -> 'b option) -> 'b option
  val map_inplace : 'a t -> f:('a -> 'a) -> unit
  val singleton : 'a -> 'a t
  val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
  val concat_mapi : 'a t -> f:(int -> 'a -> 'b t) -> 'b t
  val concat : 'a t t -> 'a t
  val concat_list : 'a t list -> 'a t
  val append_list : 'a t -> 'a list -> unit
  val append : 'a t -> 'a t -> unit
  val to_sequence : 'a t -> 'a Sequence.t
  val clear : 'a t -> unit
  val reverse : 'a t -> 'a t
  val reverse_inplace : 'a t -> unit
  val zip_exn : 'a t -> 'b t -> ('a * 'b) t

  val binary_search
    :  ?end_:int
    -> 'a t
    -> f:('a -> int)
    -> which:[ `First_equal | `First_gt | `Last_lt | `Last_le | `First_ge ]
    -> 'a option
end

module type Vec = sig
  module type S = S

  module Make (Arg : Elt) : S with module Elt = Arg

  module Value : S_value
end
