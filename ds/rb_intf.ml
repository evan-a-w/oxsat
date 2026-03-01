open! Core

module type Key = sig
  type t

  val compare : t -> t -> int
  val create_for_rb : unit -> t
end

module type Value = sig
  type t

  val create_for_rb : unit -> t
end

module type S = sig
  module Key : Key
  module Value : Value

  type t

  val create : unit -> t
  val insert : t -> key:Key.t -> data:Value.t -> unit
  val mem : t -> Key.t -> bool
  val remove : t -> Key.t -> unit
  val iter : t -> f:(key:Key.t -> data:Value.t -> unit) -> unit
  val iteri : t -> f:(key:Key.t -> data:Value.t -> unit) -> unit

  module Kv_option : sig
    type t = (Key.t * Value.t) option

    val none : unit -> t
    val some : Key.t * Value.t -> t
    val is_none : t -> bool
    val is_some : t -> bool
    val value : t -> default:(Key.t * Value.t) -> Key.t * Value.t
    val value_exn : t -> Key.t * Value.t

    module Optional_syntax : sig
      module Optional_syntax : sig
        val is_none : t -> bool
        val unsafe_value : t -> Key.t * Value.t
      end
    end
  end

  val find : t -> Key.t -> Kv_option.t
  val min : t -> Kv_option.t
  val max : t -> Kv_option.t
  val pop_min : t -> Kv_option.t
  val pop_max : t -> Kv_option.t
  val find_exn : t -> Key.t -> Value.t
  val min_exn : t -> Key.t * Value.t
  val max_exn : t -> Key.t * Value.t
  val pop_min_exn : t -> Key.t * Value.t
  val pop_max_exn : t -> Key.t * Value.t
  val fold : t -> init:'acc -> f:(acc:'acc -> key:Key.t -> data:Value.t -> 'acc) -> 'acc

  val fold_or_null
    :  t
    -> init:'acc option
    -> f:(done_:bool ref -> acc:'acc option -> key:Key.t -> data:Value.t -> 'acc option)
    -> 'acc option

  val length : t -> int
  val is_empty : t -> bool
  val clear : t -> unit
  val to_array : t -> (Key.t * Value.t) array
  val to_keys_array : t -> Key.t array
  val of_array_exn : (Key.t * Value.t) array -> t
  val validate : t -> unit

  module Iter : sig
    type tree := t
    type t

    val create : tree -> t
    val create_from : tree -> Key.t -> t
    val next : t -> Kv_option.t
    val peek : t -> Kv_option.t
    val is_done : t -> bool
  end
end

module type Rb = sig
  module Make (Key : Key) (Value : Value) : S with module Key := Key and module Value := Value
end
