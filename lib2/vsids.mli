open! Core
open! Import

type t

val create : unit -> t
val on_new_var : t -> var:int -> unit
val add_activity : t -> literal:int -> unit
val decay : t -> unit
val remove_from_pool : t -> var:int -> unit
val add_to_pool : t -> literal:int -> unit
val choose_literal : t -> int or_null
