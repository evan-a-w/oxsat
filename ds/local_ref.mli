open! Core

type 'a t

val create : 'a -> 'a t
val get : 'a t -> 'a
val set : 'a t -> 'a -> unit
val ( ! ) : 'a t -> 'a
val ( := ) : 'a t -> 'a -> unit

module O : sig
  val ( ! ) : 'a t -> 'a
  val ( := ) : 'a t -> 'a -> unit
end
