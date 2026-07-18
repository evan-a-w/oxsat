open! Core
open! Feel.Import

(** Data needed to check a theory lemma without running its originating theory
    solver. Literal indices refer to the clause certified by the value. *)

module Euf : sig
  module Equality : sig
    type t =
      { left : Formula.any
      ; right : Formula.any
      }
    [@@deriving sexp, compare]
  end

  module Justification : sig
    type t =
      | Asserted of { clause_literal : int }
      | Congruence of
          { left : Formula.any
          ; right : Formula.any
          ; argument_equalities : Equality.t list
          }
    [@@deriving sexp, compare]
  end

  module Equality_proof : sig
    type t =
      { conclusion : Equality.t
      ; path : Justification.t list
      }
    [@@deriving sexp, compare]
  end

  type t =
    | Equality of Equality_proof.t
    | Disequality of
        { conclusion : Equality.t
        ; asserted_disequality : int
        ; left_path : Equality_proof.t
        ; right_path : Equality_proof.t
        }
  [@@deriving sexp, compare]
end

module Linear_arithmetic : sig
  module Term : sig
    type t =
      { clause_literal : int
      ; coefficient : Q.t
      }
    [@@deriving sexp, compare]
  end

  type t = { combination : Term.t list } [@@deriving sexp, compare]
end

module Integer_split : sig
  type t =
    { variable : Tvar.t
    ; floor : Q.t
    ; ceil : Q.t
    }
  [@@deriving sexp, compare]
end

module Type_theory : sig
  type t =
    { left : Type_expr.t
    ; right : Type_expr.t
    ; premise_literals : int list
    }
  [@@deriving sexp, compare]
end

module Bare_var_eq : sig
  module Le_direction : sig
    type t =
      | Left_le_right
      | Right_le_left
    [@@deriving sexp, compare]
  end

  type t =
    | Equality_implies_type_equality of Tvar.t * Tvar.t
    | Equality_implies_le of
        { left : Tvar.t
        ; right : Tvar.t
        ; direction : Le_direction.t
        }
    | Numeric_coincidence_implies_equality of Tvar.t * Tvar.t
  [@@deriving sexp, compare]
end

type t =
  | Euf of Euf.t
  | Linear_arithmetic of Linear_arithmetic.t
  | Integer_split of Integer_split.t
  | Type_theory of Type_theory.t
  | Bare_var_eq of Bare_var_eq.t
[@@deriving sexp, compare]
