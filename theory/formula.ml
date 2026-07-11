open! Core
open! Feel.Import

type _ t =
  (* always used *)
  | Var : Tvar.t -> _ t
  | Eq : 'a t * 'a t -> 'a t
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
  | Type_of : 'a t -> [> `Type ] t
  | Type_app : Tvar.t * 'a t -> ([> `Type ] as 'a) t
  (* Linear arithmetic *)
  | Const : Q.t -> [> `La ] t
  | Scale_const : Q.t * 'a t -> ([> `La ] as 'a) t
  | Add : 'a t * 'a -> ([> `La ] as 'a) t
  | Compare :
      (* no eq, cuz that's already above *)
      'a t
      * [ `Le | `Ge | `Lt | `Gt ]
      * 'a t
      -> ([> `La ] as 'a) t
