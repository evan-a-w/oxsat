open! Core
open! Unboxed

module type%template
  [@kind
    k
    = ( value
      , bits64
      , float64
      , immediate & value & value
      , bits64 & bits64
      , bits64 & bits64 & bits64
      , bits64 & bits64 & immediate & immediate & bits64
      , bits64 & bits64 & value & value & bits64
      , (value & value & bits64) & bits64 & bits64 )] Elt = sig
  type t : k mod external_

  val create_for_vec : unit -> t
end

module type%template [@kind k = (value & value)] Elt = sig
  type t : k

  val create_for_vec : unit -> t
end

module type%template [@kind k = (value & value & value)] Elt = sig
  type t : value & (value & value)

  val create_for_vec : unit -> t
end

module type%template
  [@kind
    k
    = ( value
      , float64
      , immediate & value & value
      , value & value
      , value & value & value
      , bits64
      , bits64 & bits64
      , bits64 & bits64 & bits64
      , bits64 & bits64 & immediate & immediate & bits64
      , bits64 & bits64 & value & value & bits64
      , (value & value & bits64) & bits64 & bits64 )] S = sig
  module Elt : Elt [@kind k]

  type t

  val create : ?capacity:local_ int -> unit -> t
  val length : t -> int
  val get : t -> int -> Elt.t
  val set : t -> int -> Elt.t -> unit
  val iter : t -> f:(Elt.t -> unit) @ local -> unit
  val iteri : t -> f:(int -> Elt.t -> unit) @ local -> unit
  val iteri_rev : t -> f:(int -> Elt.t -> unit) @ local -> unit
  val iter_rev : t -> f:(Elt.t -> unit) @ local -> unit
  val fold : t -> init:'b -> f:('b -> Elt.t -> 'b) @ local -> 'b
  val foldr : t -> init:'b -> f:('b -> Elt.t -> 'b) @ local -> 'b
  val push : t -> Elt.t -> unit
  val pop_exn : t -> Elt.t
  val fill_to_length : t -> length:int -> f:(int -> Elt.t) @ local -> unit
  val take : t -> other:t -> unit
  val switch : t -> t -> unit
  val last_exn : t -> Elt.t
  val filter : t -> f:(Elt.t -> bool) @ local -> t
  val filter_inplace : t -> f:(Elt.t -> bool) @ local -> unit
  val map_inplace : t -> f:(Elt.t -> Elt.t) @ local -> unit
  val singleton : Elt.t -> t
  val append : t -> t -> unit
  val clear : t -> unit
  val reverse_inplace : t -> unit
end

module type S_value = sig
  type 'a t [@@deriving sexp]

  val create : ?capacity:int -> unit -> 'a t
  val length : 'a t @ local -> int
  val get : 'a t -> int -> 'a
  val get_opt : 'a t -> int -> 'a option
  val set : 'a t -> int -> 'a -> unit
  val iter : 'a t -> f:('a -> unit) @ local -> unit
  val iter_nested : 'a t t -> f:('a -> unit) @ local -> unit
  val iteri : 'a t -> f:(int -> 'a -> unit) @ local -> unit
  val iteri_rev : 'a t -> f:(int -> 'a -> unit) @ local -> unit
  val iter_rev : 'a t -> f:('a -> unit) @ local -> unit
  val copy : 'a t -> 'a t

  val%template fold
    :  'a t
    -> init:'b @ m
    -> f:('b @ m -> 'a -> 'b @ m) @ local
    -> 'b @ m
  [@@mode m = (global, local)]

  val foldr : 'a t -> init:'b -> f:('b -> 'a -> 'b) @ local -> 'b
  val push : 'a t -> 'a -> unit
  val pop_exn : 'a t -> 'a
  val fill_to_length : 'a t -> length:int -> f:(int -> 'a) @ local -> unit
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val sort : 'a t -> compare:('a -> 'a -> int) @ local -> unit

  (** if the vec is currently [ a ; b ] where both [a] and [b] are sorted, sort
      the whole array. [O(n)] *)
  val sort_partitioned
    :  'a t
    -> a_len:int
    -> compare:('a -> 'a -> int) @ local
    -> unit

  val fold_map
    :  'a t
    -> init:'acc
    -> f:('acc -> 'a -> 'acc * 'b) @ local
    -> 'b t

  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val to_array : 'a t -> 'a array
  val mem : 'a t -> 'a -> compare:('a -> 'a -> int) -> bool
  val take : 'a t -> other:'a t -> unit
  val switch : 'a t -> 'a t -> unit
  val last : 'a t -> 'a option
  val last_exn : 'a t -> 'a
  val filter : 'a t -> f:('a -> bool) @ local -> 'a t
  val filter_map : 'a t -> f:('a -> 'b option) @ local -> 'b t
  val filter_inplace : 'a t -> f:('a -> bool) @ local -> unit
  val filter_map_inplace : 'a t -> f:('a -> 'a option) @ local -> unit
  val findi : 'a t -> f:(int -> 'a -> 'b option) @ local -> 'b option
  val map_inplace : 'a t -> f:('a -> 'a) @ local -> unit
  val singleton : 'a -> 'a t
  val concat_map : 'a t -> f:('a -> 'b t) @ local -> 'b t
  val concat_mapi : 'a t -> f:(int -> 'a -> 'b t) @ local -> 'b t
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
    :  ?end_:local_ int
    -> 'a t
    -> f:('a -> int) @ local
    -> which:[ `First_equal | `First_gt | `Last_lt | `Last_le | `First_ge ]
    -> 'a option @ local
end

module type Vec = sig
  module type%template
    [@kind
      k
      = ( value
        , float64
        , value & value
        , bits64
        , bits64 & bits64
        , immediate & value & value
        , value & value & value
        , bits64 & bits64 & bits64
        , bits64 & bits64 & immediate & immediate & bits64
        , bits64 & bits64 & value & value & bits64
        , (value & value & bits64) & bits64 & bits64 )] S = S [@kind k]

  module%template
    [@kind
      k
      = ( value
        , float64
        , value & value
        , bits64
        , bits64 & bits64
        , value & value & value
        , immediate & value & value
        , bits64 & bits64 & bits64
        , bits64 & bits64 & immediate & immediate & bits64
        , bits64 & bits64 & value & value & bits64
        , (value & value & bits64) & bits64 & bits64 )] Make
      (Arg : Elt
    [@kind k]) : S [@kind k] with module Elt = Arg

  module Value : S_value
end
