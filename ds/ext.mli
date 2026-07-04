open! Core
module Typerep = Typerep_lib.Std.Typerep

module Pool : sig
  type 'a t

  val create : ?chunk_size:int -> 'a Typerep.t -> default:'a -> 'a t
  val typerep : 'a t -> 'a Typerep.t
  val chunk_size : _ t -> int
  val outstanding : _ t -> int
end

type 'a pool = 'a Pool.t
type 'a t

val create_pool : ?chunk_size:int -> 'a Typerep.t -> default:'a -> 'a pool
val alloc : 'a pool -> 'a t
val alloc_set : 'a pool -> 'a @ local -> 'a t
val free : 'a t -> unit
val set : 'a t -> 'a @ local -> unit
val get : 'a t -> 'a
val is_freed : _ t -> bool
