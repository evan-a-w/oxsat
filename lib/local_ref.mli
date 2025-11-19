open! Core

type ('a : immediate) t

val create : 'a @ local -> 'a t @ local
val get : 'a t @ local -> 'a @ local
val set : 'a t @ local -> 'a @ local -> unit
val ( ! ) : 'a t @ local -> 'a @ local
val ( := ) : 'a t @ local -> 'a @ local -> unit

module O : sig
  val ( ! ) : 'a t @ local -> 'a @ local
  val ( := ) : 'a t @ local -> 'a @ local -> unit
end
