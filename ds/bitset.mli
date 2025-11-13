open! Core
open! Unboxed

[%%template:
type t [@@deriving sexp]

val create : ?size:int -> unit -> t
val copy : t @ local -> t
val set : t @ local -> int -> unit
val clear : t @ local -> int -> unit
val get : t @ local -> int -> bool
val toggle : t @ local -> int -> unit
val clear_all : t @ local -> unit
val set_all : t @ local -> up_to:int -> unit
val popcount : t @ local -> int
val find_first_set : t @ local -> start_pos:int -> int or_null
val find_first_clear : t @ local -> start_pos:int -> int or_null
val land_inplace : dest:t @ local -> t @ local -> t @ local -> unit
val land_ : t @ local -> t @ local -> t
val lor_inplace : dest:t @ local -> t @ local -> t @ local -> unit
val lor_ : t -> t -> t
val lxor_inplace : dest:t -> t -> t -> unit
val lxor_ : t -> t -> t
val diff_inplace : dest:t @ local -> t @ local -> t @ local -> unit
val diff : t @ local -> t @ local -> t
val lnot_inplace : t -> unit
val lnot_ : t -> t
val is_empty : t -> bool
val iter_set_bits : t -> f:(int -> unit) @ local -> unit
val capacity : t -> int
val to_set_bits_array : t -> int array
val of_set_bits_array : int array -> t

val fold_set_bits
  :  t @ local
  -> init:('acc : k) @ m
  -> f:(('acc : k) @ m -> int @ m -> ('acc : k) @ m) @ local
  -> ('acc : k) @ m
[@@kind k = (value_or_null, bits64)]
[@@alloc a @ m = (stack_local, heap_global)]

(* for some reason above doesn't work... *)
val fold_set_bits_or_null
  :  t @ local
  -> init:'a or_null @ local
  -> f:('a or_null @ local -> int -> 'a or_null @ local) @ local
  -> 'a or_null @ local]
