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

(** The currently asserted truth value of a [`Le] atom (in the normalized form
    it was asserted with), or [None] if unasserted. Lets [Bare_var_eq] emit only
    bridge lemmas that aren't already satisfied. *)
val le_atom_value : t -> Atom.t -> bool option

(** Registers a Nelson-Oppen shared tvar whose LA-implied equalities need
    propagating to the other theories. *)
val add_tvar_to_check_for_equality : t -> tvar:Tvar.t -> unit

(** Pairs of registered shared tvars whose assignments coincide in the current
    simplex model. Only meaningful right after {!maybe_get_lemma} returned
    [`Consistent]. Coincidence is a model-based heuristic, not an implied
    equality: pairs can coincide spuriously (e.g. fresh simplex vars default to
    zero), in which case the bridge clauses injected for them (see
    [Bare_var_eq]) cost clauses and search but not correctness, since each pair
    is bridged at most once. A refinement would be to probe the implication in
    simplex first and only case-split for integer vars. *)
val equality_candidates : t -> (Tvar.t * Tvar.t) list

val all_numeric_vars : t -> Tvar.t list
