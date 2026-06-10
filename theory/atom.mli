open! Core
open! Feel.Import

(** General atomic formulas shared across theories. Currently only equality
    between [Term.t]s, the atom kind used by the theory of uninterpreted
    functions. *)
type t = [ `Eq of Term.t * Term.t ] [@@deriving sexp, compare, hash]

(** Orders the two sides of [`Eq] into a canonical order, so that
    semantically identical atoms compare equal regardless of argument
    order. *)
val normalize : t -> t

include Comparable.S with type t := t
include Hashable.S with type t := t
