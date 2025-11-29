open! Core
open! Unboxed

module type%template
  [@kind k = (value, bits64, bits64 & bits64, bits64 & float64)] Key = sig
  type t : k mod external_

  val compare : t -> t -> int
  val create_for_rb : unit -> t
end

module type%template
  [@kind v = (value, bits64, bits64 & bits64, bits64 & float64)] Value = sig
  type t : v mod external_

  val create_for_rb : unit -> t
end

module type%template
  [@kind
    k = (value, bits64, bits64 & bits64, bits64 & float64)
    , v = (value, bits64, bits64 & bits64)] S = sig
  module Key : Key [@kind k]
  module Value : Value [@kind v]

  type t

  val create : unit -> t
  val insert : t -> key:Key.t -> data:Value.t -> unit
  val mem : t -> Key.t -> bool
  val remove : t -> Key.t -> unit
  val iter : t -> f:(key:Key.t -> data:Value.t -> unit) @ local -> unit
  val iteri : t -> f:(key:Key.t -> data:Value.t -> unit) @ local -> unit

  module Kv_option :
    Optional_pair.S [@kind k v] with type Fst.t = Key.t and type Snd.t = Value.t

  val find : t -> Key.t -> Kv_option.t
  val min : t -> Kv_option.t
  val max : t -> Kv_option.t
  val pop_min : t -> Kv_option.t
  val pop_max : t -> Kv_option.t
  val find_exn : t -> Key.t -> Value.t
  val min_exn : t -> #(Key.t * Value.t)
  val max_exn : t -> #(Key.t * Value.t)
  val pop_min_exn : t -> #(Key.t * Value.t)
  val pop_max_exn : t -> #(Key.t * Value.t)

  val fold
    :  t
    -> init:'acc
    -> f:(acc:'acc -> key:Key.t -> data:Value.t -> 'acc) @ local
    -> 'acc

  val fold_or_null
    :  t
    -> init:'acc or_null
    -> f:
         (done_:bool Local_ref.t @ local
          -> acc:'acc or_null
          -> key:Key.t
          -> data:Value.t
          -> 'acc or_null)
       @ local
    -> 'acc or_null

  val length : t -> int
  val is_empty : t -> bool
  val clear : t -> unit

  val%template to_array : t -> #(Key.t * Value.t) array @ m
  [@@alloc a @ m = (stack_local, heap_global)]

  val of_array_exn : #(Key.t * Value.t) array -> t
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
  module%template
    [@kind
      k = (value, bits64, bits64 & bits64, bits64 & float64)
      , v = (value, bits64, bits64 & bits64)] Make
      (Key : Key
    [@kind k])
      (Value : Value
    [@kind v]) : S [@kind k v] with module Key := Key and module Value := Value
end
