open! Core
open! Unboxed

type t : bits64 mod external_ [@@deriving sexp]

val null : unit -> t
val is_null : t -> bool
val of_int : int -> t
val to_int : t -> int
val equal : t -> t -> bool

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

module Private : sig
  val chunk : chunk_bits:int -> t -> int
  val idx : chunk_bits:int -> t -> int
  val create : chunk_bits:int -> chunk:int -> idx:int -> t
end
