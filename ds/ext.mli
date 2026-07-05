open! Core
module Typerep = Typerep_lib.Std.Typerep

module Pool : sig
  type 'a t

  val typerep : 'a t -> 'a Typerep.t
  val chunk_size : _ t -> int
  val outstanding : _ t -> int
  val create : ?chunk_size:int -> 'a Typerep.t -> default:'a -> 'a t
  val create_unchecked : ?chunk_size:int -> default:'a -> unit -> 'a t
end

type 'a t

val alloc : 'a Pool.t -> 'a t
val alloc_set : 'a Pool.t -> 'a @ local -> 'a t
val free : 'a t -> unit
val set : 'a t -> 'a @ local -> unit
val get : 'a t -> 'a
val is_freed : _ t -> bool
