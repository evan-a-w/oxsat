open! Core
open! Import

module Core_step : sig
  type t =
    | Asserted of Formula.any
    | Theory_lemma of Formula.any
    | Quantifier_instance of
        { (* The originating universal axiom's body, over its bound vars. *)
          body : Formula.any
        ; (* The substitution that produced this instance. *)
          bound_values : (Tvar.t * Formula.any) list
        ; (* [body] with [bound_values] applied. *)
          instance : Formula.any
        }
  [@@deriving sexp_of]
end

type t =
  | Sat of { model : Model.t }
  | Unsat of
      { core : Core_step.t list
      ; proof : Proof.t option [@sexp.option]
      }
[@@deriving sexp_of]
