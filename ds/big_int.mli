open! Core

(** Arbitrary-precision signed integers.

    Canonical representation: a sign ([1] or [-1]) and little-endian limbs in
    base [2^15]. Zero is always [sign = 1] with no limbs: there is no negative
    zero and no leading zero limbs, so [equal], [hash], [compare], and the sexp
    representation all agree with numeric equality. All arithmetic is exact and
    overflow-free (products of two limbs fit in a machine [int] even on 32-bit
    platforms). *)

type t

val sexp_of_t : t -> Sexp.t
val t_of_sexp : Sexp.t -> t
val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val zero : t
val one : t
val minus_one : t
val of_int : int -> t

(** [Some n] if [t] fits in an [int], [None] otherwise. *)
val to_int : t -> int option

(** Parses an optionally-signed decimal string. Raises on malformed input. *)
val of_string : string -> t

(** Decimal representation, no leading zeros (["0"] for zero). *)
val to_string : t -> string

(** [-1], [0], or [1]. *)
val sign' : t -> int

val sign : t -> [ `Neg | `Zero | `Pos ]
val is_zero : t -> bool
val is_even : t -> bool
val abs : t -> t
val neg : t -> t
val succ : t -> t
val pred : t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t

(** Truncating division: [div_rem a b = (q, r)] with [q] rounded toward zero,
    [r] sharing the sign of [a], [a = q * b + r], and [abs r < abs b]. This
    matches [Int.(/)] and [Int.rem] semantics. Raises on [b = 0]. *)
val div_rem : t -> t -> t * t

val ( / ) : t -> t -> t
val ( % ) : t -> t -> t

(** Greatest common divisor of the absolute values of the arguments; never
    negative. [gcd zero zero = zero]. *)
val gcd : t -> t -> t

val min : t -> t -> t
val max : t -> t -> t
