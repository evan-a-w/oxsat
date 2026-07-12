open! Core
open! Feel.Import

(** What each theory has determined about a single [Tvar.t] in a satisfying
    model. A tvar may be constrained by more than one theory at once (e.g. its
    type and its numeric value), so any subset of these fields may be present. *)
type t =
  { type_ : Type_expr.t option
  ; numeric : Simplex.Q_eps.t option
  ; euf_repr : Formula.any option
  (** The representative term of this tvar's EUF equivalence class, if it
      differs from the tvar itself (i.e. it's been unified with something). *)
  }
[@@deriving sexp_of]
