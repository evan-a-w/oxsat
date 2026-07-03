open! Core
open! Feel.Import

(** Decision procedure for linear arithmetic over [Tvar.t] variables, combining
    {!Simplex} (for the rationals) with a branch-and-bound search (for [Tvar.t]s
    asserted to be integral via [`Type_eq]). *)

module Atom : sig
  type t =
    [ `Le of Linear_expr.t * Q.t
    | `Type_eq of Type_expr.t * Type_expr.t
    ]
  [@@deriving sexp, compare, hash]
end

type t

val create : unit -> t
val assert_atom : t -> decision_level:int -> atom:Atom.t -> value:bool -> unit
val undo : t -> to_decision_level_excl:int -> unit

(** [`Lemma literals] is a list of [(atom, value)] such that asserting all of
    them is inconsistent, i.e. the SAT solver may learn the clause that at least
    one [(atom, not value)] must hold. *)
val maybe_get_lemma : t -> [ `Consistent | `Lemma of (Atom.t * bool) list ]

(** The solved simplex assignment for [tvar], if it has been registered (i.e.
    appears in some asserted [`Le] atom). Only meaningful after
    [maybe_get_lemma] has returned [`Consistent]. *)
val assignment : t -> tvar:Tvar.t -> Simplex.Q_eps.t option

val all_numeric_vars : t -> Tvar.t list
