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

module Op : sig
  type t =
    [ `Eq
    | `Le
    | `Ge
    ]
  [@@deriving sexp, compare, hash]
end

module Snapshot : sig
  type t
end

type t [@@deriving sexp_of]
type constraint_ [@@deriving sexp, compare, equal, hash]

val add_var : t -> int
val add_constraint : t -> (Q.t * int) list * Op.t * Q.t -> constraint_
val remove_constraint : t -> constraint_:constraint_ -> unit
val solve : t -> [ `Sat | `Unsat ]
val snapshot_assignments : t -> Snapshot.t

(* if [solve] returned [`Unsat], may restore to a snapshot before calling
   [solve] again (which one might do after calling [remove_constraint] some
   number of times) *)
val restore_assignments : t -> Snapshot.t -> unit
val assignment : t -> var:int -> Q.t
val new_assignments : t -> Q.t Int.Hash_queue.t
