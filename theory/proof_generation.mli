open! Core
open! Import

type t

val create : unit -> t
val assert_formula : t -> Formula.any -> unit
val push : t -> unit
val pop : t -> unit

(** Returns [None] when the active assertions require theory reasoning rather
    than propositional reasoning over theory atoms. *)
val unsat_proof : t -> Proof.t option
