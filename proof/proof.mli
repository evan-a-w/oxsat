open! Core
open! Theory_core
module Id = Proof_id
module Atom = Proof_atom
module Boolean = Proof_boolean
module Literal = Proof_literal
module Clause = Proof_clause
module Extension = Proof_extension
module Theory_certificate = Proof_theory_certificate
module Refutation = Refutation

(** A solver-independent, human-facing proof DAG. Every step proves a
    {!Formula.quantified} (a ground {!Formula.any} embeds via
    {!Formula.widen_quantified}); step and assumption IDs are indices into the
    corresponding arrays and may only refer backwards when the proof is checked.

    Quantified formulas appear as assumptions and in the quantifier kernel rules
    below; all boolean, equality, and refutation reasoning stays over ground
    formulas. *)

module Assumption : sig
  type t =
    { name : string option
    ; formula : Formula.quantified
    }
  [@@deriving sexp, compare]
end

module Rewrite_direction : sig
  type t =
    | Left_to_right
    | Right_to_left
  [@@deriving sexp, compare]
end

type proof =
  { assumptions : Assumption.t array
  ; steps : step array
  ; conclusion : Proof_id.Step.t
  }

and kernel_rule =
  | Propositional
  | Equality_refl
  | Equality_symm
  | Equality_trans
  | Congruence
  | Rewrite of
      { direction : Rewrite_direction.t
      ; path : int list
      }
  | Forall_instantiation of
      { (* premise [∀bound. body]; conclusion [body] with each bound variable
           replaced per [bound_values]. The conclusion may still be quantified.
           Every bound variable must be instantiated. *)
        bound_values : (Tvar.t * Formula.any) list
      }
  | Exists_elim of
      { (* premise [∃bound. body]; conclusion [body] with each bound variable
           replaced by its witness per [skolems]. The conclusion may still be
           quantified, subject to the eigenvariable condition: no Skolem symbol
           occurs in the premise, in any of the proof's assumptions, in another
           existential-elimination conclusion unless it is exactly the same
           conclusion, or in the proof's final conclusion (the last enforced
           after checking all steps). *)
        skolems : (Tvar.t * Formula.any) list
      }
  (** Premise [body] with each bound variable replaced per [witnesses];
      conclusion [∃bound. body]. Every bound variable must be witnessed. There
      is no eigenvariable freshness condition. *)
  | Exists_intro of { witnesses : (Tvar.t * Formula.any) list }
  (** Conclusion [∀bound. body]; [subproof] proves [body] with each bound
      variable replaced by the corresponding eigenvariable. [imports] maps
      subproof assumption index to an earlier outer step with the same formula.
      No eigenvariable may occur in any subproof assumption, so the imported
      facts do not constrain those variables; they are arbitrary, and the body
      holds for all values. Nested introductions compose by checking the same
      local condition at each subproof. *)
  | Forall_intro of
      { eigenvariables : (Tvar.t * Tvar.t) list
      ; subproof : proof
      ; imports : Proof_id.Step.t array
      }

and justification =
  | Assumption of Proof_id.Assumption.t
  | Kernel of
      { rule : kernel_rule
      ; premises : Proof_id.Step.t array
      }
  | By_refutation of
      { premises : Proof_id.Step.t array
      ; refutation : Refutation.t
      }

and step =
  { name : string option
  ; conclusion : Formula.quantified
  ; justification : justification
  }
[@@deriving sexp, compare]

module Kernel_rule : sig
  (** Small rules intended to be checked directly, without invoking the SMT
      solver. The conclusion and premises determine the details of all rules
      except the location and direction of a rewrite. *)
  type t = kernel_rule =
    | Propositional
    | Equality_refl
    | Equality_symm
    | Equality_trans
    | Congruence
    | Rewrite of
        { direction : Rewrite_direction.t
        ; path : int list
        }
    | Forall_instantiation of { bound_values : (Tvar.t * Formula.any) list }
    | Exists_elim of { skolems : (Tvar.t * Formula.any) list }
    | Exists_intro of { witnesses : (Tvar.t * Formula.any) list }
    | Forall_intro of
        { eigenvariables : (Tvar.t * Tvar.t) list
        ; subproof : proof
        ; imports : Proof_id.Step.t array
        }
  [@@deriving sexp, compare]
end

module Justification : sig
  (** [By_refutation] proves a step by refuting the cited earlier conclusions
      together with the negation of the new conclusion. *)
  type t = justification =
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
  type t = step =
    { name : string option
    ; conclusion : Formula.quantified
    ; justification : Justification.t
    }
  [@@deriving sexp, compare]
end

type t = proof =
  { assumptions : Assumption.t array
  ; steps : step array
  ; conclusion : Proof_id.Step.t
  }
[@@deriving sexp, compare]

val check : t -> unit Or_error.t

(** Renders the proof as an indented, human-readable listing: assumptions and
    steps in mathematical notation, with each refutation's extension
    definitions, clauses, and per-step reasons (theory certificates spelled
    out), and each universal-introduction subproof nested below its enclosing
    step. For reading and expect tests, not machine consumption. *)
val to_string_hum : t -> string

val check_theory_certificate
  :  ?datatype_env:Datatype.Env.t
  -> clause:Clause.t
  -> Theory_certificate.t
  -> unit Or_error.t
