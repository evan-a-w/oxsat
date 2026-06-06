open! Core

type t [@@deriving sexp_of]

val create : ?capacity:int -> unit -> t

(** Add a new element and return its id. *)
val add : t -> int

(** [find t x] returns the representative of [x]'s class.
    Auto-expands if [x] is beyond the current capacity. *)
val find : t -> int -> int

(** [union t x y] merges the classes of [x] and [y].
    Returns [true] if they were in different classes (i.e. a new merge occurred).
    Auto-expands as needed. *)
val union : t -> int -> int -> bool

val same_class : t -> int -> int -> bool

(** Number of elements added so far. *)
val size : t -> int
