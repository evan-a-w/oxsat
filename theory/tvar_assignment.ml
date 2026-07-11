open! Core
open! Feel.Import

type t =
  { type_ : Type_expr.t option
  ; numeric : Simplex.Q_eps.t option
  ; euf_repr : Formula.Uf.Term.t option
  }
[@@deriving sexp_of]
