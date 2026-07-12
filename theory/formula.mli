open! Core
open! Feel.Import

(** Propositional formulas over theory atoms. *)
type _ t =
  (* always used *)
  | Var : Tvar.t -> [> `Term ] t
  | Eq : 'a t * 'a t -> ([> `Atom ] as 'a) t
  (* boolean structure *)
  | True : [> `Boolean ] t
  | False : [> `Boolean ] t
  | Not : 'a t -> ([> `Boolean ] as 'a) t
  | And : 'a t list -> ([> `Boolean ] as 'a) t
  | Or : 'a t list -> ([> `Boolean ] as 'a) t
  (* UF *)
  | App : Tvar.t * 'a t list -> ([> `Uf ] as 'a) t
  (* Types *)
  | Bool : [> `Type ] t
  | Int : [> `Type ] t
  | Float : [> `Type ] t
  | Type : [> `Type ] t
  | Function_type : 'a t * 'a t -> ([> `Type ] as 'a) t
  | Type_of : 'a t -> ([> `Type ] as 'a) t
  | Type_app : Tvar.t * 'a t list -> ([> `Type ] as 'a) t
  (* Linear arithmetic *)
  | La_const : Q.t -> [> `La ] t
  | La_scale_const : Q.t * 'a t -> ([> `La ] as 'a) t
  | La_add : 'a t * 'a t -> ([> `La ] as 'a) t
  | La_compare :
      (* no eq, cuz that's already above *)
      'a t
      * [ `Le | `Ge | `Lt | `Gt ]
      * 'a t
      -> ([> `La ] as 'a) t
[@@deriving sexp, compare, hash, equal]

type any = [ `Boolean | `Uf | `Type | `La | `Term | `Atom ] t
[@@deriving sexp, compare, hash, equal]

module Op : sig
  type t =
    | Var of Tvar.t
    | Eq
    | True
    | False
    | Not
    | And
    | Or
    | App of Tvar.t
    | Bool
    | Int
    | Float
    | Type
    | Function_type
    | Type_of
    | Type_app of Tvar.t
    | La_const of Q.t
    | La_scale_const of Q.t
    | La_add
    | La_compare of [ `Le | `Ge | `Lt | `Gt ]
  [@@deriving sexp, compare, hash, equal]
end

val op : 'a t -> Op.t
val args : 'a t -> any list

module Uf : Uninterpreted_functions_intf.S with type Term.t = [ `Uf | `Term ] t
