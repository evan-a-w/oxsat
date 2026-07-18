open! Core

(** Canonical non-tautological clauses. Construction normalizes atoms, sorts
    literals, removes duplicates, and reports complementary literals as a
    tautology. *)

type t = private Proof_literal.t array [@@deriving sexp, compare]

val create : Proof_literal.t list -> [ `Clause of t | `Tautology ]
val empty : t
val literals : t -> Proof_literal.t array
