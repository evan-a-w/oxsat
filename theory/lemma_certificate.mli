open! Core
open! Import

(** A theory lemma's proof certificate expressed over the lemma's atoms rather
    than over positional clause-literal indices, so each sub-theory can describe
    it without knowing how the lemma clause will be ordered.
    {!to_theory_certificate} resolves the atoms against a concrete clause. *)

module Euf : sig
  module Justification : sig
    type t =
      | Asserted of { disequality : Atom.t }
      | Congruence of
          { left : Formula.any
          ; right : Formula.any
          ; argument_equalities : Proof.Theory_certificate.Euf.Equality.t list
          }
  end

  module Equality_proof : sig
    type t =
      { conclusion : Proof.Theory_certificate.Euf.Equality.t
      ; path : Justification.t list
      }
  end

  type t =
    | Equality of Equality_proof.t
    | Disequality of
        { conclusion : Proof.Theory_certificate.Euf.Equality.t
        ; asserted_disequality : Atom.t
        ; left_path : Equality_proof.t
        ; right_path : Equality_proof.t
        }
end

module Euf_map : sig
  (** Rewrites the atoms a EUF certificate references by clause index, leaving
      structural terms untouched. *)
  val map_atoms : f:(Atom.t -> Atom.t) -> Euf.t -> Euf.t
end

module Linear_arithmetic : sig
  type t = { combination : (Atom.t * Q.t) list }
end

module Type_theory : sig
  type t =
    { left : Type_expr.t
    ; right : Type_expr.t
    ; premises : Atom.t list
    }
end

type t =
  | Euf of Euf.t
  | Linear_arithmetic of Linear_arithmetic.t
  | Integer_split of Proof.Theory_certificate.Integer_split.t
  | Type_theory of Type_theory.t
  | Bare_var_eq of Proof.Theory_certificate.Bare_var_eq.t

(** Resolves the certificate's atom references to positional indices in
    [clause], producing the certificate the refutation checker validates. Raises
    if a cited atom is absent from [clause]. *)
val to_theory_certificate : Proof.Clause.t -> t -> Proof.Theory_certificate.t
