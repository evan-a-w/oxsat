open! Core
open! Feel.Import

type t =
  { type_ : Type_expr.t option
  ; numeric : Simplex.Q_eps.t option
  ; euf_repr : [ `Uf ] Formula.t option
  }

(* Written by hand rather than [@@deriving sexp_of]: ppx treats [Formula.t]'s
   phantom parameter like an ordinary type argument, so it would generate a call
   assuming [Formula.sexp_of_t] takes an extra per-argument converter (as it
   would for a real container type) -- it doesn't. *)
let sexp_of_t { type_; numeric; euf_repr } =
  [%message
    ""
      (type_ : Type_expr.t option)
      (numeric : Simplex.Q_eps.t option)
      ~euf_repr:(Option.map euf_repr ~f:Formula.sexp_of_t : Sexp.t option)]
;;
