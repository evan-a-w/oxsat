open! Core
open! Import

type t [@@deriving sexp]

val reset : t -> unit
val is_seen : t -> var:int -> bool
val mark_seen : t -> var:int -> unit
val clear_seen : t -> var:int -> unit
