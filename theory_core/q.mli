open! Core

(** Exact rational numbers, stored as a normalized [num/den] pair of arbitrary
    precision integers ([den > 0], [gcd(|num|, den) = 1], [num = 0 => den = 1]). *)
type t [@@deriving sexp, hash, equal]

val compare : t -> t -> int
val zero : t
val one : t
val of_int : int -> t
val of_int64 : Int64.t -> t
val of_bigint : Bigint.t -> t
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
val num : t -> Bigint.t

val den : t -> Bigint.t

(** For debug printing only; not exact. *)
val to_float : t -> float

val is_integral : t -> bool
val floor : t -> t
val ceil : t -> t
