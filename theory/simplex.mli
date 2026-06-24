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

val create : unit -> t
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

(** If there is a basic variable currently out of bounds (i.e. the most recent
    [solve] returned [`Unsat] or the current assignment is infeasible), folds
    over the variables that witness the infeasibility of that row. For the basic
    variable itself, yields [(var_id, `Le)] or [(var_id, `Ge)] for whichever
    bound it violates. For each nonbasic variable in the row with a nonzero
    coefficient, yields [(var_id, `Le)] or [(var_id, `Ge)] for the bound that is
    blocking the pivot needed to fix the basic variable. Returns [None] if no
    row is currently infeasible. *)
val fold_conflict_row
  :  t
  -> f:(var_id:int -> bound_side:[ `Le | `Ge ] -> unit)
  -> unit option
