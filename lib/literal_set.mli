open! Core
open! Import

type t

val create : random_state:Random.State.t -> t

(** gives out a random literal *)
val pop_one : t -> int or_null

val insert : t -> literal:int -> unit
val remove : t -> literal:int -> unit
