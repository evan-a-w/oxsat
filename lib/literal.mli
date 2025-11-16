open! Core
open! Import

type t : bits64 mod external_ [@@deriving sexp]

val equal : t -> t -> bool

val create : var:int -> value:bool -> t
val var : t @ local -> int
val value : t @ local -> bool
val compare : t @ local -> t @ local -> int
val to_int : t @ local -> int
val of_int : int -> t
val negate : t -> t

module Option : sig
  type value = t
  type t : bits64 mod everything

  val none : unit -> t [@@zero_alloc]
  val some : value -> t [@@zero_alloc]
  val unchecked_some : value -> t [@@zero_alloc]
  val some_is_representable : value -> bool [@@zero_alloc]
  val is_none : t -> bool [@@zero_alloc]
  val is_some : t -> bool [@@zero_alloc]
  val value : t -> default:value -> value [@@zero_alloc]
  val unchecked_value : t -> value [@@zero_alloc]

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool [@@zero_alloc]
      val unsafe_value : t -> value [@@zero_alloc]
    end
  end
end
