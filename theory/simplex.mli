open! Core
open! Feel.Import

module Maybe_bound : sig
  type 'a t =
    | Unbounded
    | Bounded of 'a
  [@@deriving sexp, compare, hash]
end

(** [{ value; eps_coeff }] represents [value + (eps_coeff * delta)], for a fixed
    but symbolic infinitesimal [delta]. Used to encode strict bounds
    ([`Lt]/[`Gt]) without native strict-inequality support in the tableau. *)
module Q_eps : sig
  type t =
    { value : Q.t
    ; eps_coeff : Q.t
    }
  [@@deriving sexp, compare, hash]

  val of_q : Q.t -> t

  (** True iff this is exactly an integer, i.e. has no symbolic-infinitesimal
      offset and an integral [value]. A value like [3 + delta] (from a strict
      bound [x > 3]) is not integral, even though its [value] is. *)
  val is_integral : t -> bool
end

module Bound : sig
  type t =
    { le : Q_eps.t Maybe_bound.t
    ; ge : Q_eps.t Maybe_bound.t
    }
  [@@deriving sexp]
end

module Op : sig
  type t =
    [ `Eq
    | `Le
    | `Ge
    | `Lt
    | `Gt
    ]
  [@@deriving sexp, compare, hash]
end

module Snapshot : sig
  type t
end

module Constraint : sig
  type t [@@deriving sexp, compare, equal, hash]

  include Hashable.S with type t := t
end

type t [@@deriving sexp_of]

val create : unit -> t
val add_var : t -> int
val add_constraint : t -> (Q.t * int) list * Op.t * Q.t -> Constraint.t
val remove_constraint : t -> constraint_:Constraint.t -> unit
val solve : t -> [ `Sat | `Unsat of Constraint.t list ]
val snapshot_assignments : t -> Snapshot.t

(** if [solve] returned [`Unsat], may restore to a snapshot before calling
    [solve] again (which one might do after calling [remove_constraint] some
    number of times) *)
val restore_assignments : t -> Snapshot.t -> unit

val assignment : t -> var:int -> Q_eps.t
val new_assignments : t -> Q_eps.t Int.Hash_queue.t

(** If enough constraints have been tombstoned by [remove_constraint], rebuild
    the tableau from scratch to reclaim space. For each decision var whose
    internal id changed, calls [~update ~old_var ~new_var] so callers can update
    any tables keyed on those ids. No-op when below the threshold. *)
val maybe_rebuild : t -> update:(old_var:int -> new_var:int -> unit) -> unit
