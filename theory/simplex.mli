open! Core
open! Feel.Import

module Maybe_bound : sig
  type 'a t =
    | Unbounded
    | Bounded of 'a
  [@@deriving sexp, compare, hash]
end

module Bound : sig
  type t =
    { le : Q.t Maybe_bound.t
    ; ge : Q.t Maybe_bound.t
    }
  [@@deriving sexp]
end

module Sum : sig
  type t =
    { vars : (Q.t * int) Iarray.t
    ; const : Q.t
    }
  [@@deriving sexp, compare, hash]
end

module Op : sig
  type t =
    [ `Eq
    | `Le
    | `Ge
    ]
  [@@deriving sexp, compare, hash]
end

type t [@@deriving sexp]

val add_nonbasic : t -> int
val add_constraint : t -> Sum.t * Op.t * Sum.t -> unit
val restore_last_satisfying_assignment : t -> unit
val solve : t -> [ `Sat | `Unsat ]
