open! Core

module type Key = sig
  type t

  val hash : t -> int
  val equal : t -> t -> bool
  val create_for_hash_table : unit -> t
end

module type Value = sig
  type t

  val create_for_hash_table : unit -> t
end

module type S = sig
  module Key : Key
  module Value : Value

  type t

  val create : ?capacity:int -> ?max_load_percent:int -> unit -> t
  val length : t -> int
  val is_empty : t -> bool
  val load_factor : t -> float
  val clear : t -> unit
  val insert : t -> key:Key.t -> data:Value.t -> unit
  val mem : t -> Key.t -> bool
  val remove : t -> Key.t -> unit

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
  val find_exn : t -> Key.t -> Value.t
  val iter : t -> f:(key:Key.t -> data:Value.t -> unit) -> unit
  val iteri : t -> f:(key:Key.t -> data:Value.t -> unit) -> unit
  val fold : t -> init:'acc -> f:(acc:'acc -> key:Key.t -> data:Value.t -> 'acc) -> 'acc
  val to_array : t -> (Key.t * Value.t) array
  val to_keys_array : t -> Key.t array
  val choose_arbitrarily : t -> Kv_option.t
end

module type Hash_table = sig
  module Make (Key : Key) (Value : Value) : S with module Key := Key and module Value := Value
end
