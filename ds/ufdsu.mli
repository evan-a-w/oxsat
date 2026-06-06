open! Core

type t [@@deriving sexp_of]

module Level : sig
  type t [@@deriving sexp_of]
end

val create : ?capacity:int -> unit -> t
val add : t -> int

(** [find t x] returns the representative of [x]'s class. O(log n). Auto-expands
    if [x] is beyond the current capacity. *)
val find : t -> int -> int

(** [union t x y] merges the classes of [x] and [y] and records the merge on the
    internal trail. Returns [true] if they were in different classes.
    Auto-expands as needed. *)
val union : t -> int -> int -> bool

val same_class : t -> int -> int -> bool

(** Number of elements added so far. *)
val size : t -> int

(** Mark the current union history. *)
val save : t -> Level.t

(** Undo all unions performed since [level] was saved. *)
val restore : t -> Level.t -> unit
