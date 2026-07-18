open! Core

(** A solver-independent, human-facing proof DAG. Every step proves a
    {!Formula.any}; step and assumption IDs are indices into the corresponding
    arrays and may only refer backwards when the proof is checked. *)

module Assumption : sig
  type t =
    { name : string option
    ; formula : Formula.any
    }
  [@@deriving sexp, compare]
end

module Rewrite_direction : sig
  type t =
    | Left_to_right
    | Right_to_left
  [@@deriving sexp, compare]
end

module Kernel_rule : sig
  (** Small rules intended to be checked directly, without invoking the SMT
      solver. The conclusion and premises determine the details of all rules
      except the location and direction of a rewrite. *)
  type t =
    | Propositional
    | Equality_refl
    | Equality_symm
    | Equality_trans
    | Congruence
    | Rewrite of
        { direction : Rewrite_direction.t
        ; path : int list
        }
  [@@deriving sexp, compare]
end

module Justification : sig
  (** [By_refutation] proves a step by refuting the cited earlier conclusions
      together with the negation of the new conclusion. *)
  type t =
    | Assumption of Proof_id.Assumption.t
    | Kernel of
        { rule : Kernel_rule.t
        ; premises : Proof_id.Step.t array
        }
    | By_refutation of
        { premises : Proof_id.Step.t array
        ; refutation : Refutation.t
        }
  [@@deriving sexp, compare]
end

module Step : sig
  type t =
    { name : string option
    ; conclusion : Formula.any
    ; justification : Justification.t
    }
  [@@deriving sexp, compare]
end

type t =
  { assumptions : Assumption.t array
  ; steps : Step.t array
  ; conclusion : Proof_id.Step.t
  }
[@@deriving sexp, compare]
