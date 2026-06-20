open! Core
open! Feel.Import

module Tvar_and_type = struct
  type t =
    { tvar : Tvar.t
    ; type_ : Type_expr.Base.t
    }
  [@@deriving sexp, compare, equal, hash]

  include functor Comparable.Make
  include functor Hashable.Make
end

module Trail_entry = struct
  type t =
    { decision_level : int
    ; var : int
    ; lb : Simplex.constraint_
    ; ub : Simplex.constraint_
    }
end

type t =
  { simplex_var : int Tvar_and_type.Table.t
  ; simplex : Simplex.t
  ; trail : Trail_entry.t Vec.Value.t
  }
