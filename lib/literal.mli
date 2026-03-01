open! Core

type t = int [@@deriving sexp]

val equal : t -> t -> bool
val create : var:int -> value:bool -> t
val var : t -> int
val value : t -> bool
val compare : t -> t -> int
val to_int : t -> int
val of_int : int -> t
val negate : t -> t

module Option : sig
  type value = t
  type t = value option

  val none : unit -> t
  val some : value -> t
  val unchecked_some : value -> t
  val some_is_representable : value -> bool
  val is_none : t -> bool
  val is_some : t -> bool
  val value : t -> default:value -> value
  val unchecked_value : t -> value

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool
      val unsafe_value : t -> value
    end
  end
end
