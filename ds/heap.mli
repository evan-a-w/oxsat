open! Core

(** A binary min-heap, ordered by the [compare] function given at creation. *)
type 'a t [@@deriving sexp_of]

val create : ?capacity:int -> compare:('a -> 'a -> int) -> unit -> 'a t
val of_list : compare:('a -> 'a -> int) -> 'a list -> 'a t
val length : 'a t -> int
val is_empty : 'a t -> bool
val clear : 'a t -> unit
val push : 'a t -> 'a -> unit

(** Returns the minimal element without removing it. *)
val peek : 'a t -> 'a option

val peek_exn : 'a t -> 'a

(** Removes and returns the minimal element. *)
val pop : 'a t -> 'a option

val pop_exn : 'a t -> 'a

(** [to_list t] returns the elements in heap (array) order, not sorted order. *)
val to_list : 'a t -> 'a list
