open! Core

module type S = sig
  type t [@@deriving sexp, compare, equal, hash]

  val of_int_exn : int -> t
  val to_int : t -> int
end

module Make () : S = struct
  type t = int [@@deriving sexp_of, compare, equal, hash]

  let of_int_exn t =
    if t < 0 then invalid_arg "proof IDs must be non-negative" else t
  ;;

  let t_of_sexp sexp = Int.t_of_sexp sexp |> of_int_exn
  let to_int t = t
end

module Assumption = Make ()
module Step = Make ()
module Extension = Make ()
module Refutation_step = Make ()
