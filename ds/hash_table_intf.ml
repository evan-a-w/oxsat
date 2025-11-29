open! Core
open! Unboxed

module type%template
  [@kind k = (value, bits64, bits64 & bits64, bits64 & float64)] Key = sig
  type t : k mod external_

  val hash : t -> int
  val equal : t -> t -> bool
  val create_for_hash_table : unit -> t
end

module type%template
  [@kind v = (value, bits64, bits64 & bits64, bits64 & float64)] Value = sig
  type t : v mod external_

  val create_for_hash_table : unit -> t
end

module type%template
  [@kind
    k = (value, bits64, bits64 & bits64, bits64 & float64)
    , v = (value, bits64, bits64 & bits64)] S = sig
  module Key : Key [@kind k]
  module Value : Value [@kind v]

  type t

  val create
    :  ?capacity:local_ int
    -> ?max_load_percent:local_ int
    -> unit
    -> t

  val length : t -> int
  val is_empty : t -> bool
  val load_factor : t -> float
  val clear : t -> unit
  val insert : t -> key:Key.t -> data:Value.t -> unit
  val mem : t -> Key.t -> bool
  val remove : t -> Key.t -> unit

  module Kv_option :
    Optional_pair.S [@kind k v] with type Fst.t = Key.t and type Snd.t = Value.t

  val find : t -> Key.t -> Kv_option.t
  val find_exn : t -> Key.t -> Value.t

  val iter : t -> f:(key:Key.t -> data:Value.t -> unit) @ local -> unit
  val iteri : t -> f:(key:Key.t -> data:Value.t -> unit) @ local -> unit

  val fold
    :  t
    -> init:'acc
    -> f:(acc:'acc -> key:Key.t -> data:Value.t -> 'acc) @ local
    -> 'acc
end

module type Hash_table = sig
  module%template
    [@kind
      k = (value, bits64, bits64 & bits64, bits64 & float64)
      , v = (value, bits64, bits64 & bits64)] Make
      (Key : Key
    [@kind k])
      (Value : Value
    [@kind v]) : S [@kind k v] with module Key := Key and module Value := Value
end
