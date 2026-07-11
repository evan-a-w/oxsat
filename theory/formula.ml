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
  (* Linear arithmetic (prefixed with [La_] so we can re-use similar stuff for
     eg. bitvectors) *)
  | La_const : Q.t -> [> `La ] t
  | La_scale_const : Q.t * 'a t -> ([> `La ] as 'a) t
  | La_add : 'a t * 'a t -> ([> `La ] as 'a) t
  | La_compare :
      (* no eq, cuz that's already above *)
      'a t
      * [ `Le | `Ge | `Lt | `Gt ]
      * 'a t
      -> ([> `La ] as 'a) t

(* [ppx_sexp_conv] can't derive sexp converters for GADTs, so this is written by
   hand; sexp-of only, since nothing needs to parse a [Formula.t] back. *)
let rec sexp_of_t : type a. a t -> Sexp.t =
  fun formula ->
  let node tag args = Sexp.List (Sexp.Atom tag :: args) in
  match formula with
  | Var v -> node "Var" [ [%sexp_of: Tvar.t] v ]
  | Eq (a, b) -> node "Eq" [ sexp_of_t a; sexp_of_t b ]
  | True -> Sexp.Atom "True"
  | False -> Sexp.Atom "False"
  | Not f -> node "Not" [ sexp_of_t f ]
  | And fs -> node "And" [ [%sexp_of: Sexp.t list] (List.map fs ~f:sexp_of_t) ]
  | Or fs -> node "Or" [ [%sexp_of: Sexp.t list] (List.map fs ~f:sexp_of_t) ]
  | App (f, args) ->
    node
      "App"
      [ [%sexp_of: Tvar.t] f
      ; [%sexp_of: Sexp.t list] (List.map args ~f:sexp_of_t)
      ]
  | Bool -> Sexp.Atom "Bool"
  | Int -> Sexp.Atom "Int"
  | Float -> Sexp.Atom "Float"
  | Type -> Sexp.Atom "Type"
  | Function_type (a, b) -> node "Function_type" [ sexp_of_t a; sexp_of_t b ]
  | Type_of f -> node "Type_of" [ sexp_of_t f ]
  | Type_app (f, a) -> node "Type_app" [ [%sexp_of: Tvar.t] f; sexp_of_t a ]
  | La_const q -> node "La_const" [ [%sexp_of: Q.t] q ]
  | La_scale_const (q, a) ->
    node "La_scale_const" [ [%sexp_of: Q.t] q; sexp_of_t a ]
  | La_add (a, b) -> node "La_add" [ sexp_of_t a; sexp_of_t b ]
  | La_compare (a, op, b) ->
    node
      "La_compare"
      [ sexp_of_t a; [%sexp_of: [ `Le | `Ge | `Lt | `Gt ]] op; sexp_of_t b ]
;;

type any = [ `Boolean | `Uf | `Type | `La ] t

let sexp_of_any = sexp_of_t
