open! Core
open! Import

type t

type time_bound =
  [ `Unlimited
  | `Bounded of int
  ]

exception Timeout

val%template solve
  :  ?time_bound:time_bound
  -> ?assumptions:int array @ local
  -> t
  -> Sat_result.t @ m
[@@alloc a @ m = (heap_global, stack_local)]

val create
  :  ?theory:Theory.Packed.t
  -> ?random_state:Random.State.t
  -> ?debug:bool @ local
  -> ?produce_proofs:bool
  -> unit
  -> t

(** A clause participating in a resolution/RUP refutation of the most recent
    [Unsat]. [clause_idx] is the clause's stable index (or [-1] for the
    synthesized final empty clause). [Rup] marks a clause derived by reverse
    unit propagation (a learned clause, or the final empty clause). Only
    populated when [~produce_proofs:true] was passed to {!create}. *)
module Refutation_clause : sig
  module Reason : sig
    type t =
      | Input
      | Theory
      | Rup
  end

  type t =
    { clause_idx : int
    ; literals : int array
    ; reason : Reason.t
    }
end

(** The clauses transitively responsible for the last [Unsat], topologically
    ordered (antecedents before dependents), ending in the empty clause. [None]
    when proofs were not enabled or no [Unsat] has occurred. *)
val last_refutation : t -> Refutation_clause.t list option

val create_with_formula
  :  ?theory:Theory.Packed.t
  -> ?debug:bool @ local
  -> int array array
  -> [ `Ok of t | `Unsat of Sat_result.Core_clause.t list ]

val add_clause
  :  t
  -> clause:int array
  -> [ `Ok | `Unsat of Sat_result.Core_clause.t list ]

val stats : t -> Stats.t
