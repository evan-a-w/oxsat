open! Core
open! Import

type t

val create : unit -> t

(** idempotent *)
val on_new_var : t -> var:int -> unit

val add_activity : t -> literal:Literal.t -> unit
val decay : t -> unit
val remove_from_pool : t -> var:int -> unit
val add_to_pool : t -> var:int -> unit
val choose_literal : t -> Literal.Option.t
