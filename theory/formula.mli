open! Core
open! Feel.Import

(** Propositional formulas over theory atoms. *)
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
  | La_const : Q.t -> [> `La ] t
  | La_scale_const : Q.t * 'a t -> ([> `La ] as 'a) t
  | La_add : 'a t * 'a t -> ([> `La ] as 'a) t
  | La_compare :
      (* no eq, cuz that's already above *)
      'a t
      * [ `Le | `Ge | `Lt | `Gt ]
      * 'a t
      -> ([> `La ] as 'a) t

val sexp_of_t : 'a t -> Sexp.t

(** Inverse of [sexp_of_t]. Since [t]'s type parameter is a phantom tag not
    recorded in the sexp, this can only produce whichever (necessarily open) tag
    the parsed shape happens to support -- e.g. it can never produce a bare
    [Bool] instantiated at [[ \`Uf ]], since [Bool]'s own type only allows
    [[> \`Type ]]. Used monomorphically at each concrete call site. *)
val t_of_sexp : Sexp.t -> [> `Boolean | `La | `Type | `Uf ] t

val compare : 'a t -> 'a t -> int
val equal : 'a t -> 'a t -> bool
val hash : 'a t -> int
val hash_fold_t : Hash.state -> 'a t -> Hash.state

(** A [t] closed over every tag, i.e. one that could be any kind of formula
    (boolean, UF, type, or linear-arithmetic). Used where a formula is stored or
    returned without statically knowing (or caring) which theories it touches --
    e.g. asserted formulas and unsat-core reasons. *)
type any = [ `Boolean | `Uf | `Type | `La ] t

val sexp_of_any : any -> Sexp.t
val any_of_sexp : Sexp.t -> any
val compare_any : any -> any -> int
val equal_any : any -> any -> bool
val hash_any : any -> int
val hash_fold_any : Hash.state -> any -> Hash.state

(** The theory of equality over uninterpreted functions
    ({!Uninterpreted_functions}), instantiated over the UF-tagged fragment of
    [t] ([Var] and [App]) directly, so there's no separate term type to convert
    to and from. *)
module Uf : Uninterpreted_functions_intf.S with type Term.t = [ `Uf ] t
