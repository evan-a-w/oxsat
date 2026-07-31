open! Core
open! Feel.Import

type any_theory =
  [ `Boolean
  | `Uf
  | `Type
  | `La
  | `Array
  | `Adt
  | `Term
  | `Atom
  ]

(** [any_theory] plus [`Quantified]. Kept separate from [any_theory] so that
    functions typed over [any] (e.g. {!Formula_egraph_uf.add_term},
    [Encoding.encode], [Model.check]) are statically incapable of receiving a
    [Forall]/[Exists] -- see {!Quantifier_elaboration} for how those get turned
    into plain [any] before reaching ground-only consumers. *)
type any_quantified_theory =
  [ any_theory
  | `Quantified
  ]

(** Propositional formulas over theory atoms. *)
type _ t =
  (* always used *)
  | Var : Tvar.t -> [> `Term ] t
  | Eq : 'a t * 'a t -> ([> `Atom ] as 'a) t
  | Ite : any_theory t * 'a t * 'a t -> 'a t
  (* boolean structure *)
  | True : [> `Boolean ] t
  | False : [> `Boolean ] t
  | Not : 'a t -> ([> `Boolean ] as 'a) t
  | And : 'a t list -> ([> `Boolean ] as 'a) t
  | Or : 'a t list -> ([> `Boolean ] as 'a) t
  (* Quantifiers. [body]/[triggers] are plain [any_theory t], not the
     polymorphic ['a t] used elsewhere in this GADT: a Forall/Exists body can't
     itself contain a further quantifier (no alternation in v1). Tagged
     [`Quantified], not [`Boolean], so it never unifies with [any]. *)
  | Forall :
      Tvar.t list * any_theory t list list * any_theory t
      -> ([> `Quantified ] as 'a) t
  | Exists : Tvar.t list * any_theory t -> ([> `Quantified ] as 'a) t
  (* UF *)
  | App : Tvar.t * 'a t list -> ([> `Uf ] as 'a) t
  (* Arrays *)
  | Select : 'a t * 'a t -> ([> `Array ] as 'a) t
  | Store : 'a t * 'a t * 'a t -> ([> `Array ] as 'a) t
  (* Algebraic datatypes *)
  | Datatype_constructor :
      Datatype.Constructor.t * 'a t list
      -> ([> `Adt ] as 'a) t
  | Datatype_selector : Datatype.Selector.t * 'a t -> ([> `Adt ] as 'a) t
  | Datatype_tester : Datatype.Constructor.t * 'a t -> ([> `Boolean ] as 'a) t
  (* Types *)
  | Bool : [> `Type ] t
  | Int : [> `Type ] t
  | Float : [> `Type ] t
  | Type : [> `Type ] t
  | Function_type : 'a t * 'a t -> ([> `Type ] as 'a) t
  | Array_type : 'a t * 'a t -> ([> `Type ] as 'a) t
  | Type_of : 'a t -> ([> `Type ] as 'a) t
  | Type_var : Tvar.t -> [> `Type ] t
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

module Theory : sig
  type _ t =
    | Uf : [ `Uf | `Atom | `Term ] t
    | Type : [ `Type | `Atom | `Term ] t
    | La : [ `La | `Atom | `Term ] t
    | Array : [ `Array | `Atom | `Term ] t
    | Adt : [ `Adt | `Atom | `Term ] t
    | Boolean : [ `Boolean | `Atom | `Term ] t
    | Shared : any_theory t

  type 'a inner = 'a t

  module Packed : sig
    type t = T : 'a inner -> t [@@deriving sexp_of, equal]

    (** Least upper bound: equal theories stay themselves, two different
        theories become [Shared]. *)
    val join : t -> t -> t

    (** [includes t theory]: whether a tvar with membership [t] participates in
        [theory]. [Shared] participates in every theory. *)
    val includes : t -> t -> bool
  end
end

type any = any_theory t [@@deriving sexp, compare, hash, equal]

(** No [of_sexp]/round-trip: quantified formulas are built programmatically, not
    parsed, so only the printing direction is provided. *)
type quantified = any_quantified_theory t
[@@deriving sexp_of, compare, hash, equal]

(** Widens the phantom theory tag without changing the formula. *)
val widen : 'a t -> any

(** Like {!widen}, but into the wider [`Quantified]-inclusive [quantified]. *)
val widen_quantified : 'a t -> quantified

(** [Some g] when [q] contains no quantifier node -- so it is already a plain
    ground formula -- and [None] otherwise. The inverse direction of
    {!widen_quantified} for the binder-free case. *)
val to_any : quantified -> any option

(** Parses the [sexp_of_quantified] representation. *)
val quantified_of_sexp : Sexp.t -> quantified

(** Every [Tvar.t] mentioned anywhere in [q]: term variables, function/type
    application heads, and quantifier bound-variable lists. Used for the
    eigenvariable freshness side condition of existential elimination. *)
val tvars : quantified -> Tvar.Set.t

module Any : sig
  type t = any [@@deriving sexp, compare, hash]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Quantified : sig
  type t = quantified [@@deriving sexp_of, compare, hash]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

module Op : sig
  type t =
    | Var of Tvar.t
    | Eq
    | Ite
    | True
    | False
    | Not
    | And
    | Or
    | App of Tvar.t
    | Select
    | Store
    | Datatype_constructor of Datatype.Constructor.t
    | Datatype_selector of Datatype.Selector.t
    | Datatype_tester of Datatype.Constructor.t
    | Bool
    | Int
    | Float
    | Type
    | Function_type
    | Array_type
    | Type_of
    | Type_var of Tvar.t
    | Type_app of Tvar.t
    | La_const of Q.t
    | La_scale_const of Q.t
    | La_add
    | La_compare of [ `Le | `Ge | `Lt | `Gt ]
    | Forall of Tvar.t list
    | Exists of Tvar.t list
  [@@deriving sexp, compare, hash, equal]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

val op : 'a t -> Op.t
val args : 'a t -> any list
val make_opt : op:Op.t -> args:any list -> any option
val make : op:Op.t -> args:any list -> any

(** Capture-free substitution over a ground term: replaces each [Var v] for [v]
    in [subst] with its mapped replacement, leaving everything else structurally
    unchanged. *)
val substitute : any Tvar.Map.t -> any -> any

(** Rewrites term-level [Ite] nodes into Boolean structure at their enclosing
    formula position, preserving binder scope when applied to quantifier bodies
    before ground encoding. The result contains no [Ite] nodes. *)
val expand_term_ites : any -> any
