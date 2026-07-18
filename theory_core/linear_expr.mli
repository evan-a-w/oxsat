open! Core
open! Feel.Import

(** A linear expression [const + sum_v coeffs[v] * v] over [Tvar.t] variables. *)
type t =
  { coeffs : Q.t Map.M(Tvar).t
  ; const : Q.t
  }
[@@deriving sexp, compare, hash]

include Comparable.S with type t := t
include Hashable.S with type t := t

val zero : t
val const : Q.t -> t

(** Coefficient [1] on [v]. *)
val var : Tvar.t -> t

val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val scale : Q.t -> t -> t
val neg : t -> t
val is_zero : t -> bool

(** [primitive t] returns [(t', factor)] with [factor > 0] and
    [t = scale factor t'], where [t'] has integer coefficients and constant with
    gcd 1 (or [t' = zero] if [t = zero]). Used to canonicalize atoms, e.g. so
    [2x+2y<=4] and [x+y<=2] normalize the same. *)
val primitive : t -> t * Q.t
