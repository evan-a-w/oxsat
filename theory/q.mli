open! Core

(** Exact rational numbers, stored as a normalized [num/den] pair of [int]s
    ([den > 0], [gcd(|num|, den) = 1], [num = 0 => den = 1]).

    Arithmetic operations reduce via [Int.gcd] after every step. Plain [int]
    numerators/denominators can overflow on pathological inputs; this is a
    known, accepted limitation for now. *)
type t [@@deriving sexp, hash, equal]

val compare : t -> t -> int
val zero : t
val one : t
val of_int : int -> t
val ( + ) : t @ local -> t @ local -> t
val ( - ) : t @ local -> t @ local -> t
val ( * ) : t @ local -> t @ local -> t
val ( / ) : t @ local -> t @ local -> t
val neg : t @ local -> t
val abs : t @ local -> t

(** [-1], [0], or [1]. *)
val sign' : t -> int

val sign : t -> [ `Neg | `Zero | `Pos ]
val is_zero : t -> bool
val min : t -> t -> t
val max : t -> t -> t

(** Numerator and (always-positive) denominator of the normalized form. *)
val num : t -> int

val den : t -> int

(** For debug printing only; not exact. *)
val to_float : t -> float

val is_integral : t -> bool
