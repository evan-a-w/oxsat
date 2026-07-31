open! Core
open! Feel.Import
open! Theory_core

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

module Array : sig
  type t =
    | Read_over_write_same_index of
        { array : Formula.any
        ; index : Formula.any
        ; value : Formula.any
        }
    | Read_over_write_different_index of
        { array : Formula.any
        ; written_index : Formula.any
        ; written_value : Formula.any
        ; read_index : Formula.any
        }
    | Extensionality of
        { left : Formula.any
        ; right : Formula.any
        ; witness : Formula.any
        ; type_premises : (Tvar.t * Type_expr.t) list
        }
  [@@deriving sexp, compare]
end

module Adt : sig
  module Cycle_edge : sig
    type t =
      { constructor_term : Formula.any
      ; field : Formula.any
      }
    [@@deriving sexp, compare]
  end

  type t =
    | Injectivity of
        { constructor : Datatype.Constructor.t
        ; left_args : Formula.any list
        ; right_args : Formula.any list
        ; field_index : int
        }
    | Disjointness of
        { left_constructor : Datatype.Constructor.t
        ; left_args : Formula.any list
        ; right_constructor : Datatype.Constructor.t
        ; right_args : Formula.any list
        }
    | Tester of
        { tester_constructor : Datatype.Constructor.t
        ; argument : Formula.any
        ; witness_constructor : Datatype.Constructor.t
        ; witness_args : Formula.any list
        ; value : bool
        }
    | Selector of
        { selector : Datatype.Selector.t
        ; argument : Formula.any
        ; constructor_args : Formula.any list
        }
    | Acyclicity of { cycle : Cycle_edge.t list }
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
  | Array of Array.t
  | Adt of Adt.t
  | Bare_var_eq of Bare_var_eq.t
[@@deriving sexp, compare]
