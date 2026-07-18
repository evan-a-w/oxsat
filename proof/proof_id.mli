open! Core

(** Abstract non-negative indices used by serialized proof DAGs. *)

module type S = sig
  type t [@@deriving sexp, compare, equal, hash]

  val of_int_exn : int -> t
  val to_int : t -> int
end

module Assumption : S
module Step : S
module Extension : S
module Refutation_step : S
