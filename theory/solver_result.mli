open! Core
open! Import

module Core_step : sig
  type t =
    | Asserted of Formula.any
    | Theory_lemma of Formula.any
  [@@deriving sexp_of]
end

type t =
  | Sat of { model : Model.t }
  | Unsat of
      { core : Core_step.t list
      ; proof : Proof.t option [@sexp.option]
      }
[@@deriving sexp_of]
