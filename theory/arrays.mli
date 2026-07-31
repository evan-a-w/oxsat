open! Core
open! Import

(** Lazy instantiation of McCarthy arrays over the shared congruence closure.

    [Arrays] does not maintain a second weak-equivalence data structure. It
    watches the ground terms and equality atoms already registered with
    {!Formula_egraph_uf}, and emits only the array lemmas that are relevant to
    those terms:

    - read-over-write at the written index is instantiated once for every
      registered [store a i v] term;
    - read-over-write at a different index is instantiated for every registered
      [select (store a i v) j] term. The lemma is guarded by the equality
      [i = j], so the SAT/combination layer decides whether the index terms are
      equal instead of eagerly enumerating all index pairs;
    - extensionality is instantiated for a registered disequality between
      equivalence classes that are known to be arrays. An equivalence class is
      known to be an array if one of its currently registered members occurs in
      array position (or is itself a [store] term), or if one of its variable
      members currently has a declared [Array_type] sort. A fresh witness
      variable is introduced for each such class pair, and the emitted clause is
      the usual [a = b \/ select a k <> select b k], guarded by negative
      [has_type] premises for any side whose array-ness came only from a
      declared sort. To avoid pure overhead, declared sorts alone do not make an
      otherwise-unused pair relevant: at least one of the two classes must also
      have an array-shaped member.

    Lemmas are globally valid, so the "already emitted" sets are intentionally
    not undone when the SAT solver backtracks. The decision-level [undo] hook is
    still present for the combined-theory interface; no decision-level-local
    array facts are stored here. Equivalence classes and declared sorts can both
    change under backtracking, so array class membership is recomputed when
    looking for an extensionality lemma instead of cached in this module, and
    scoped declared-sort facts only appear as guarded premises in retained
    clauses.

    [maybe_get_lemma] is called on every theory propagation even for problems
    that never mention arrays, so it starts from an O(1) check: if no
    array-shaped term has ever appeared in a registered atom and no declared
    array sort has ever been seen, no lemma is possible and it returns
    immediately without scanning the egraph. Both signals are only ever added to
    and are re-checked on every call, so an array term or declared sort that
    arrives later disables the fast path rather than latching it off. *)

type t

val create : unit -> t

(** Registers an equality atom that may mention array terms. The caller should
    also register the same atom with {!Formula_egraph_uf}; this module only uses
    the callback to discover which terms are array terms and which disequalities
    are candidates for extensionality. *)
val add_atom : t -> atom:Atom.Equality.t -> unit

val maybe_get_lemma
  :  t
  -> egraph:Formula_egraph_uf.t
  -> get_type:(Tvar.t -> Type_expr.t option)
  -> [ `Consistent | `Lemma of (Atom.Equality.t * bool) list ]

val last_certificate : t -> Lemma_certificate.Array.t option
val undo : t -> to_decision_level_excl:int -> unit
