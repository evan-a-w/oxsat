open! Core
open! Import

(** Lazy lemmas for declared algebraic datatypes over the shared congruence
    closure.

    Formula nodes carry constructor, selector, and tester metadata, but every
    ADT shape used by the solver must be present in the current declaration
    environment. This module watches equality atoms for ADT-shaped terms and
    emits only lemmas relevant to currently registered egraph terms:

    - equal applications of the same constructor imply equality of each field;
    - applications of distinct constructors of the same datatype are disjoint;
    - testers are true on their constructor, false on other constructors of the
      same datatype, and mutually exclusive for non-ground subjects;
    - positive testers reconstruct constructor terms when all fields have
      declared selectors;
    - selectors project their owning constructor's field, and are left
      underspecified on other constructors;
    - finite values cannot form constructor cycles, including multi-step cycles
      through equalities.

    [maybe_get_lemma] starts with an O(1) no-ADT fast path, so ADT-free problems
    do not scan the egraph. *)

type t

val create : ?env:Datatype.Env.t -> unit -> t
val push : t -> unit
val pop : t -> unit

val declare
  :  t
  -> ?guard:Atom.Equality.t
  -> Datatype.Declaration.t
  -> unit Or_error.t

val env : t -> Datatype.Env.t
val validate_formula : t -> Formula.any -> unit Or_error.t

val type_constraints
  :  t
  -> Formula.any
  -> (Tvar.t * Type_expr.t) list Or_error.t

val datatype_observations : t -> Datatype.Datatype.Set.t Formula.Any.Map.t
val add_atom : t -> atom:Atom.Equality.t -> unit

val maybe_get_lemma
  :  t
  -> egraph:Formula_egraph_uf.t
  -> [ `Consistent | `Lemma of (Atom.Equality.t * bool) list ]

val last_certificate : t -> Lemma_certificate.Adt.t option
val undo : t -> to_decision_level_excl:int -> unit
