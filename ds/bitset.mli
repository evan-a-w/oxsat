open! Core

type t [@@deriving sexp]

val create : ?size:int -> unit -> t
val copy : t -> t
val set : t -> int -> unit
val clear : t -> int -> unit
val get : t -> int -> bool
val toggle : t -> int -> unit
val clear_all : t -> unit
val set_all : t -> up_to:int -> unit
val popcount : t -> int
val find_first_set : t -> start_pos:int -> int option
val find_first_clear : t -> start_pos:int -> int option
val land_inplace : dest:t -> t -> t -> unit
val land_ : t -> t -> t
val lor_inplace : dest:t -> t -> t -> unit
val lor_ : t -> t -> t
val lxor_inplace : dest:t -> t -> t -> unit
val lxor_ : t -> t -> t
val diff_inplace : dest:t -> t -> t -> unit
val diff : t -> t -> t
val lnot_inplace : t -> unit
val lnot_ : t -> t
val is_empty : t -> bool
val iter_set_bits : t -> f:(int -> unit) -> unit
val capacity : t -> int
val to_set_bits_array : t -> int array
val of_set_bits_array : int array -> t
val fold_set_bits : t -> init:'acc -> f:('acc -> int -> 'acc) -> 'acc
val fold_set_bits_or_null : t -> init:'acc option -> f:(done_:bool ref -> 'acc option -> int -> 'acc option) -> 'acc option
