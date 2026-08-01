open! Core
open! Feel.Import
open! Theory_core

module Euf = struct
  module Equality = struct
    type t =
      { left : Formula.any
      ; right : Formula.any
      }
    [@@deriving sexp, compare]
  end

  module Justification = struct
    type t =
      | Asserted of { clause_literal : int }
      | Congruence of
          { left : Formula.any
          ; right : Formula.any
          ; argument_equalities : Equality.t list
          }
    [@@deriving sexp, compare]
  end

  module Equality_proof = struct
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

module Linear_arithmetic = struct
  module Term = struct
    type t =
      { clause_literal : int
      ; coefficient : Q.t
      }
    [@@deriving sexp, compare]
  end

  type t = { combination : Term.t list } [@@deriving sexp, compare]
end

module Integer_split = struct
  type t =
    { variable : Tvar.t
    ; floor : Q.t
    ; ceil : Q.t
    }
  [@@deriving sexp, compare]
end

module Type_theory = struct
  type t =
    { left : Type_expr.t
    ; right : Type_expr.t
    ; premise_literals : int list
    }
  [@@deriving sexp, compare]
end

module Array = struct
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

module Adt = struct
  module Cycle_edge = struct
    type t =
      { constructor_term : Formula.any
      ; field : Formula.any
      }
    [@@deriving sexp, compare]
  end

  module Completeness_form = struct
    type t =
      | Enum_equalities
      | Testers
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
    | Tester_exclusivity of
        { left_constructor : Datatype.Constructor.t
        ; left_argument : Formula.any
        ; right_constructor : Datatype.Constructor.t
        ; right_argument : Formula.any
        }
    | Tester_reconstruction of
        { constructor : Datatype.Constructor.t
        ; argument : Formula.any
        }
    | Selector of
        { selector : Datatype.Selector.t
        ; argument : Formula.any
        ; constructor_args : Formula.any list
        }
    | Acyclicity of { cycle : Cycle_edge.t list }
    | Completeness of
        { declaration : Datatype.Declaration.t
        ; subject : Formula.any
        ; guard : Atom.Equality.t option
        ; form : Completeness_form.t
        }
  [@@deriving sexp, compare]
end

module Bare_var_eq = struct
  module Le_direction = struct
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
