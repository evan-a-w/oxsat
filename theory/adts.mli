open! Core
open! Import

(** Lazy lemmas for algebraic datatypes over the shared congruence closure.

    Formula nodes carry self-describing constructor, selector, and tester
    metadata; there is intentionally no global declaration table yet. This
    module watches equality atoms for ADT-shaped terms and emits only lemmas
    relevant to currently registered egraph terms:

    - equal applications of the same constructor imply equality of each field;
    - applications of distinct constructors of the same datatype are disjoint;
    - testers are true on their constructor and false on other constructors of
      the same datatype;
    - selectors project their owning constructor's field, and are left
      underspecified on other constructors;
    - finite values cannot form constructor cycles, including multi-step cycles
      through equalities.

    [maybe_get_lemma] starts with an O(1) no-ADT fast path, so ADT-free problems
    do not scan the egraph. *)

type t

val create : unit -> t
val add_atom : t -> atom:Atom.Equality.t -> unit

val maybe_get_lemma
  :  t
  -> egraph:Formula_egraph_uf.t
  -> [ `Consistent | `Lemma of (Atom.Equality.t * bool) list ]

val last_certificate : t -> Lemma_certificate.Adt.t option
val undo : t -> to_decision_level_excl:int -> unit
