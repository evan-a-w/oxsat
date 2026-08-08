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

type any_quantified_theory =
  [ any_theory
  | `Quantified
  ]

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
  (* Quantifiers -- see formula.mli for the [quantified]/[any] split. *)
  | Forall : Tvar.t list * 'a t list list * 'a t -> ([> `Quantified ] as 'a) t
  | Exists : Tvar.t list * 'a t -> ([> `Quantified ] as 'a) t
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
  | Real : [> `Type ] t
  | Int64 : [> `Type ] t
  | Type : [> `Type ] t
  | Function_type : 'a t * 'a t -> ([> `Type ] as 'a) t
  | Array_type : 'a t * 'a t -> ([> `Type ] as 'a) t
  | Type_of : 'a t -> ([> `Type ] as 'a) t
  | Type_var : Tvar.t -> [> `Type ] t
  | Type_app : Tvar.t * 'a t list -> ([> `Type ] as 'a) t
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

module Op = struct
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
    | Real
    | Int64
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

  include functor Hashable.Make
  include functor Comparable.Make
end

let op : type a. a t -> Op.t =
  fun t ->
  match t with
  | Var v -> Var v
  | Eq _ -> Eq
  | Ite _ -> Ite
  | True -> True
  | False -> False
  | Not _ -> Not
  | And _ -> And
  | Or _ -> Or
  | Forall (bound, _, _) -> Forall bound
  | Exists (bound, _) -> Exists bound
  | App (v, _) -> App v
  | Select _ -> Select
  | Store _ -> Store
  | Datatype_constructor (constructor, _) -> Datatype_constructor constructor
  | Datatype_selector (selector, _) -> Datatype_selector selector
  | Datatype_tester (constructor, _) -> Datatype_tester constructor
  | Bool -> Bool
  | Int -> Int
  | Real -> Real
  | Int64 -> Int64
  | Type -> Type
  | Function_type _ -> Function_type
  | Array_type _ -> Array_type
  | Type_of _ -> Type_of
  | Type_var v -> Type_var v
  | Type_app (v, _) -> Type_app v
  | La_const q -> La_const q
  | La_scale_const (q, _) -> La_scale_const q
  | La_add _ -> La_add
  | La_compare (_, op, _) -> La_compare op
;;

module Theory = struct
  type _ t =
    | Uf : [ `Uf | `Atom | `Term ] t
    | Type : [ `Type | `Atom | `Term ] t
    | La : [ `La | `Atom | `Term ] t
    | Array : [ `Array | `Atom | `Term ] t
    | Adt : [ `Adt | `Atom | `Term ] t
    | Boolean : [ `Boolean | `Atom | `Term ] t
    | Shared : any_theory t

  type 'a inner = 'a t

  module Packed = struct
    type t = T : 'a inner -> t

    let sexp_of_t (T t) =
      match t with
      | Uf -> Sexp.Atom "Uf"
      | Type -> Sexp.Atom "Type"
      | La -> Sexp.Atom "La"
      | Array -> Sexp.Atom "Array"
      | Adt -> Sexp.Atom "Adt"
      | Boolean -> Sexp.Atom "Boolean"
      | Shared -> Sexp.Atom "Shared"
    ;;

    let equal (T a) (T b) =
      match a, b with
      | Uf, Uf
      | Type, Type
      | La, La
      | Array, Array
      | Adt, Adt
      | Boolean, Boolean
      | Shared, Shared -> true
      | (Uf | Type | La | Array | Adt | Boolean | Shared), _ -> false
    ;;

    let join a b = if equal a b then a else T Shared
    let includes t theory = equal t (T Shared) || equal t theory
  end
end

type any = any_theory t
type quantified = any_quantified_theory t

let widen (type a) (t : a t) : any = Obj.magic t
let widen_quantified (type a) (t : a t) : quantified = Obj.magic t
let widen_list (type a) (l : a t list) : any list = Obj.magic l

let widen_quantified_list (type a) (l : a t list) : quantified list =
  Obj.magic l
;;

let args (type a) (t : a t) : any list =
  match t with
  | Var _ -> []
  | Eq (a, b) -> [ widen a; widen b ]
  | Ite (condition, then_, else_) -> [ condition; widen then_; widen else_ ]
  | True -> []
  | False -> []
  | Not x -> [ widen x ]
  | And l -> widen_list l
  | Or l -> widen_list l
  | Forall _ -> []
  | Exists _ -> []
  | App (_, l) -> widen_list l
  | Select (array, index) -> [ widen array; widen index ]
  | Store (array, index, value) -> [ widen array; widen index; widen value ]
  | Datatype_constructor (_, args) -> widen_list args
  | Datatype_selector (_, arg) -> [ widen arg ]
  | Datatype_tester (_, arg) -> [ widen arg ]
  | Bool -> []
  | Int -> []
  | Real -> []
  | Int64 -> []
  | Type -> []
  | Function_type (a, b) -> [ widen a; widen b ]
  | Array_type (index, element) -> [ widen index; widen element ]
  | Type_of x -> [ widen x ]
  | Type_var _ -> []
  | Type_app (_, l) -> widen_list l
  | La_const _ -> []
  | La_scale_const (_, r) -> [ widen r ]
  | La_add (a, b) -> [ widen a; widen b ]
  | La_compare (a, _, b) -> [ widen a; widen b ]
;;

let quantified_args (q : quantified) : quantified list =
  match q with
  | Var _ -> []
  | Eq (a, b) -> [ widen_quantified a; widen_quantified b ]
  | Ite (condition, then_, else_) ->
    [ widen_quantified condition
    ; widen_quantified then_
    ; widen_quantified else_
    ]
  | True -> []
  | False -> []
  | Not x -> [ widen_quantified x ]
  | And l -> widen_quantified_list l
  | Or l -> widen_quantified_list l
  | Forall (_, triggers, body) ->
    List.concat_map triggers ~f:widen_quantified_list
    @ [ widen_quantified body ]
  | Exists (_, body) -> [ widen_quantified body ]
  | App (_, l) -> widen_quantified_list l
  | Select (array, index) -> [ widen_quantified array; widen_quantified index ]
  | Store (array, index, value) ->
    [ widen_quantified array; widen_quantified index; widen_quantified value ]
  | Datatype_constructor (_, args) -> widen_quantified_list args
  | Datatype_selector (_, arg) -> [ widen_quantified arg ]
  | Datatype_tester (_, arg) -> [ widen_quantified arg ]
  | Bool -> []
  | Int -> []
  | Real -> []
  | Int64 -> []
  | Type -> []
  | Function_type (a, b) -> [ widen_quantified a; widen_quantified b ]
  | Array_type (index, element) ->
    [ widen_quantified index; widen_quantified element ]
  | Type_of x -> [ widen_quantified x ]
  | Type_var _ -> []
  | Type_app (_, l) -> widen_quantified_list l
  | La_const _ -> []
  | La_scale_const (_, r) -> [ widen_quantified r ]
  | La_add (a, b) -> [ widen_quantified a; widen_quantified b ]
  | La_compare (a, _, b) -> [ widen_quantified a; widen_quantified b ]
;;

let make_opt ~(op : Op.t) ~(args : any list) : any option =
  match (op : Op.t), args with
  | Var v, [] -> Some (Var v)
  | Eq, [ a; b ] -> Some (Eq (a, b))
  | Ite, [ condition; then_; else_ ] -> Some (Ite (condition, then_, else_))
  | True, [] -> Some True
  | False, [] -> Some False
  | Not, [ a ] -> Some (Not a)
  | And, l -> Some (And l)
  | Or, l -> Some (Or l)
  (* Unlike every other op, a Forall/Exists can never be reconstructed here: the
     result would have to be typed [quantified], not [any]. *)
  | Forall _, _ -> None
  | Exists _, _ -> None
  | App v, l -> Some (App (v, l))
  | Select, [ array; index ] -> Some (Select (array, index))
  | Store, [ array; index; value ] -> Some (Store (array, index, value))
  | Datatype_constructor constructor, args ->
    if List.length args = constructor.arity
    then Some (Datatype_constructor (constructor, args))
    else None
  | Datatype_selector selector, [ arg ] ->
    Some (Datatype_selector (selector, arg))
  | Datatype_tester constructor, [ arg ] ->
    Some (Datatype_tester (constructor, arg))
  | Bool, [] -> Some Bool
  | Int, [] -> Some Int
  | Real, [] -> Some Real
  | Int64, [] -> Some Int64
  | Type, [] -> Some Type
  | Function_type, [ a; b ] -> Some (Function_type (a, b))
  | Array_type, [ index; element ] -> Some (Array_type (index, element))
  | Type_of, [ a ] -> Some (Type_of a)
  | Type_var v, [] -> Some (Type_var v)
  | Type_app v, l -> Some (Type_app (v, l))
  | La_const q, [] -> Some (La_const q)
  | La_scale_const q, [ a ] -> Some (La_scale_const (q, a))
  | La_add, [ a; b ] -> Some (La_add (a, b))
  | La_compare cmp, [ a; b ] -> Some (La_compare (a, cmp, b))
  | ( ( Var _
      | Eq
      | Ite
      | True
      | False
      | Not
      | Select
      | Store
      | Datatype_selector _
      | Datatype_tester _
      | Bool
      | Int
      | Real
      | Int64
      | Type
      | Function_type
      | Array_type
      | Type_of
      | Type_var _
      | La_const _
      | La_scale_const _
      | La_add
      | La_compare _ )
    , _ ) -> None
;;

let make ~op ~args = Option.value_exn (make_opt ~op ~args)

let rec type_expr_to_formula : Type_expr.t -> any = function
  | Var v -> Type_var v
  | Base Bool -> Bool
  | Base Int -> Int
  | Base Real -> Real
  | Base Int64 -> Int64
  | Type_of v -> Type_of (Var v)
  | App (f, args) -> Type_app (f, List.map args ~f:type_expr_to_formula)
  | Type -> Type
  | Function_type (a, b) ->
    Function_type (type_expr_to_formula a, type_expr_to_formula b)
  | Array_type (index, element) ->
    Array_type (type_expr_to_formula index, type_expr_to_formula element)
;;

module Ite_case = struct
  type t =
    { conditions : any list
    ; term : any
    }
end

let combine_ite_cases (cases_by_arg : Ite_case.t list list) =
  List.fold
    cases_by_arg
    ~init:[ [], [] ]
    ~f:(fun acc arg_cases ->
      List.concat_map acc ~f:(fun (conditions, args) ->
        List.map
          arg_cases
          ~f:(fun { Ite_case.conditions = arg_conditions; term } ->
            conditions @ arg_conditions, args @ [ term ])))
;;

let conjunct = function
  | [] -> True
  | [ formula ] -> formula
  | formulas -> And formulas
;;

let rec term_ite_cases (term : any) : Ite_case.t list =
  match term with
  | Ite (condition, then_, else_) ->
    let condition = expand_term_ites condition in
    let then_cases = term_ite_cases (widen then_) in
    let else_cases = term_ite_cases (widen else_) in
    List.map then_cases ~f:(fun case ->
      { case with conditions = condition :: case.conditions })
    @ List.map else_cases ~f:(fun case ->
      { case with conditions = Not condition :: case.conditions })
  | _ ->
    let cases_by_arg = List.map (args term) ~f:term_ite_cases in
    List.map (combine_ite_cases cases_by_arg) ~f:(fun (conditions, args) ->
      { Ite_case.conditions; term = make ~op:(op term) ~args })

and expand_term_ites (formula : any) : any =
  match term_ite_cases formula with
  | [ { conditions = []; term } ] -> term
  | cases ->
    Or
      (List.map cases ~f:(fun { conditions; term } ->
         conjunct (conditions @ [ term ])))
;;

let rec substitute (subst : any Tvar.Map.t) (term : any) : any =
  match term with
  | Var v ->
    (match Map.find subst v with
     | Some replacement -> replacement
     | None -> term)
  | _ ->
    let new_args = List.map (args term) ~f:(substitute subst) in
    make ~op:(op term) ~args:new_args
;;

let without_bound subst bound =
  List.fold bound ~init:subst ~f:(fun subst v -> Map.remove subst v)
;;

let rec tvars_of_any (acc : Tvar.Set.t) (formula : any) : Tvar.Set.t =
  let acc =
    match op formula with
    | Var v | App v | Type_var v | Type_app v -> Set.add acc v
    | _ -> acc
  in
  List.fold (args formula) ~init:acc ~f:tvars_of_any
;;

let replacement_tvars subst =
  Map.data subst
  |> List.fold ~init:Tvar.Set.empty ~f:(fun acc replacement ->
    tvars_of_any acc replacement)
;;

let rec substitute_quantified (subst : any Tvar.Map.t) (formula : quantified)
  : quantified Or_error.t
  =
  let open Or_error.Let_syntax in
  let substitute_list formulas =
    Or_error.all (List.map formulas ~f:(substitute_quantified subst))
  in
  match formula with
  | Var v ->
    (match Map.find subst v with
     | Some replacement -> Ok (widen_quantified replacement)
     | None -> Ok (Var v))
  | Eq (a, b) ->
    let%map a = substitute_quantified subst (widen_quantified a)
    and b = substitute_quantified subst (widen_quantified b) in
    Eq (a, b)
  | Ite (condition, then_, else_) ->
    let condition = substitute subst condition in
    let%map then_ = substitute_quantified subst (widen_quantified then_)
    and else_ = substitute_quantified subst (widen_quantified else_) in
    Ite (condition, then_, else_)
  | True -> Ok True
  | False -> Ok False
  | Not f ->
    let%map f = substitute_quantified subst (widen_quantified f) in
    Not f
  | And fs ->
    let%map fs = substitute_list (widen_quantified_list fs) in
    And fs
  | Or fs ->
    let%map fs = substitute_list (widen_quantified_list fs) in
    Or fs
  | Forall (bound, triggers, body) ->
    let subst = without_bound subst bound in
    let captured =
      Set.inter (Tvar.Set.of_list bound) (replacement_tvars subst)
    in
    if not (Set.is_empty captured)
    then
      Or_error.error_s
        [%message
          "substitution would capture an inner universal binder"
            (captured : Tvar.Set.t)]
    else (
      let%map triggers =
        Or_error.all
          (List.map triggers ~f:(fun trigger ->
             Or_error.all
               (List.map
                  (widen_quantified_list trigger)
                  ~f:(substitute_quantified subst))))
      and body = substitute_quantified subst (widen_quantified body) in
      Forall (bound, triggers, body))
  | Exists (bound, body) ->
    let subst = without_bound subst bound in
    let captured =
      Set.inter (Tvar.Set.of_list bound) (replacement_tvars subst)
    in
    if not (Set.is_empty captured)
    then
      Or_error.error_s
        [%message
          "substitution would capture an inner existential binder"
            (captured : Tvar.Set.t)]
    else (
      let%map body = substitute_quantified subst (widen_quantified body) in
      Exists (bound, body))
  | App (f, args) ->
    let%map args = substitute_list (widen_quantified_list args) in
    App (f, args)
  | Select (array, index) ->
    let%map array = substitute_quantified subst (widen_quantified array)
    and index = substitute_quantified subst (widen_quantified index) in
    Select (array, index)
  | Store (array, index, value) ->
    let%map array = substitute_quantified subst (widen_quantified array)
    and index = substitute_quantified subst (widen_quantified index)
    and value = substitute_quantified subst (widen_quantified value) in
    Store (array, index, value)
  | Datatype_constructor (constructor, args) ->
    let%map args = substitute_list (widen_quantified_list args) in
    Datatype_constructor (constructor, args)
  | Datatype_selector (selector, arg) ->
    let%map arg = substitute_quantified subst (widen_quantified arg) in
    Datatype_selector (selector, arg)
  | Datatype_tester (constructor, arg) ->
    let%map arg = substitute_quantified subst (widen_quantified arg) in
    Datatype_tester (constructor, arg)
  | Bool -> Ok Bool
  | Int -> Ok Int
  | Real -> Ok Real
  | Int64 -> Ok Int64
  | Type -> Ok Type
  | Function_type (a, b) ->
    let%map a = substitute_quantified subst (widen_quantified a)
    and b = substitute_quantified subst (widen_quantified b) in
    Function_type (a, b)
  | Array_type (index, element) ->
    let%map index = substitute_quantified subst (widen_quantified index)
    and element = substitute_quantified subst (widen_quantified element) in
    Array_type (index, element)
  | Type_of f ->
    let%map f = substitute_quantified subst (widen_quantified f) in
    Type_of f
  | Type_var v -> Ok (Type_var v)
  | Type_app (f, args) ->
    let%map args = substitute_list (widen_quantified_list args) in
    Type_app (f, args)
  | La_const q -> Ok (La_const q)
  | La_scale_const (q, f) ->
    let%map f = substitute_quantified subst (widen_quantified f) in
    La_scale_const (q, f)
  | La_add (a, b) ->
    let%map a = substitute_quantified subst (widen_quantified a)
    and b = substitute_quantified subst (widen_quantified b) in
    La_add (a, b)
  | La_compare (a, op, b) ->
    let%map a = substitute_quantified subst (widen_quantified a)
    and b = substitute_quantified subst (widen_quantified b) in
    La_compare (a, op, b)
;;

let rec sexp_of_t : type a. (a -> Sexp.t) -> a t -> Sexp.t =
  fun sexp_of_a formula ->
  let node tag args = Sexp.List (Sexp.Atom tag :: args) in
  (* Children of the same phantom tag [a] as [formula] itself. *)
  let sexp_of_sub a = sexp_of_t sexp_of_a a in
  let sexp_of_ground a = sexp_of_t (fun _ -> assert false) a in
  match formula with
  | Var v -> node "Var" [ [%sexp_of: Tvar.t] v ]
  | Eq (a, b) -> node "Eq" [ sexp_of_sub a; sexp_of_sub b ]
  | Ite (condition, then_, else_) ->
    node
      "Ite"
      [ sexp_of_ground condition; sexp_of_sub then_; sexp_of_sub else_ ]
  | True -> Sexp.Atom "True"
  | False -> Sexp.Atom "False"
  | Not f -> node "Not" [ sexp_of_sub f ]
  | And fs ->
    node "And" [ [%sexp_of: Sexp.t list] (List.map fs ~f:sexp_of_sub) ]
  | Or fs -> node "Or" [ [%sexp_of: Sexp.t list] (List.map fs ~f:sexp_of_sub) ]
  | Forall (bound, triggers, body) ->
    node
      "Forall"
      [ [%sexp_of: Tvar.t list] bound
      ; [%sexp_of: Sexp.t list list]
          (List.map triggers ~f:(List.map ~f:sexp_of_sub))
      ; sexp_of_sub body
      ]
  | Exists (bound, body) ->
    node "Exists" [ [%sexp_of: Tvar.t list] bound; sexp_of_sub body ]
  | App (f, args) ->
    node
      "App"
      [ [%sexp_of: Tvar.t] f
      ; [%sexp_of: Sexp.t list] (List.map args ~f:sexp_of_sub)
      ]
  | Select (array, index) ->
    node "Select" [ sexp_of_sub array; sexp_of_sub index ]
  | Store (array, index, value) ->
    node "Store" [ sexp_of_sub array; sexp_of_sub index; sexp_of_sub value ]
  | Datatype_constructor (constructor, args) ->
    node
      "Datatype_constructor"
      [ [%sexp_of: Datatype.Constructor.t] constructor
      ; [%sexp_of: Sexp.t list] (List.map args ~f:sexp_of_sub)
      ]
  | Datatype_selector (selector, arg) ->
    node
      "Datatype_selector"
      [ [%sexp_of: Datatype.Selector.t] selector; sexp_of_sub arg ]
  | Datatype_tester (constructor, arg) ->
    node
      "Datatype_tester"
      [ [%sexp_of: Datatype.Constructor.t] constructor; sexp_of_sub arg ]
  | Bool -> Sexp.Atom "Bool"
  | Int -> Sexp.Atom "Int"
  | Real -> Sexp.Atom "Real"
  | Int64 -> Sexp.Atom "Int64"
  | Type -> Sexp.Atom "Type"
  | Function_type (a, b) ->
    node "Function_type" [ sexp_of_sub a; sexp_of_sub b ]
  | Array_type (index, element) ->
    node "Array_type" [ sexp_of_sub index; sexp_of_sub element ]
  | Type_of f -> node "Type_of" [ sexp_of_sub f ]
  | Type_var v -> node "Type_var" [ [%sexp_of: Tvar.t] v ]
  | Type_app (f, args) ->
    node
      "Type_app"
      [ [%sexp_of: Tvar.t] f
      ; [%sexp_of: Sexp.t list] (List.map args ~f:sexp_of_sub)
      ]
  | La_const q -> node "La_const" [ [%sexp_of: Q.t] q ]
  | La_scale_const (q, a) ->
    node "La_scale_const" [ [%sexp_of: Q.t] q; sexp_of_sub a ]
  | La_add (a, b) -> node "La_add" [ sexp_of_sub a; sexp_of_sub b ]
  | La_compare (a, op, b) ->
    node
      "La_compare"
      [ sexp_of_sub a; [%sexp_of: [ `Le | `Ge | `Lt | `Gt ]] op; sexp_of_sub b ]
;;

let sexp_of_t_any : type a. a t -> Sexp.t =
  fun f -> sexp_of_t (fun _ -> assert false) f
;;

let sexp_of_quantified (q : quantified) : Sexp.t =
  sexp_of_t (fun _ -> assert false) q
;;

let rec contains_binder (q : quantified) : bool =
  match q with
  | Forall _ | Exists _ -> true
  | _ -> List.exists (quantified_args q) ~f:contains_binder
;;

let to_any (q : quantified) : any option =
  if contains_binder q then None else Some (widen q)
;;

let rec any_of_sexp sexp : any =
  let fail () = of_sexp_error "Formula.any_of_sexp: unexpected sexp" sexp in
  match sexp with
  | Sexp.Atom "True" -> True
  | Sexp.Atom "False" -> False
  | Sexp.Atom "Bool" -> Bool
  | Sexp.Atom "Int" -> Int
  | Sexp.Atom "Real" -> Real
  | Sexp.Atom "Float" -> Real
  | Sexp.Atom "Int64" -> Int64
  | Sexp.Atom "Type" -> Type
  | Sexp.Atom _ -> fail ()
  | Sexp.List (Sexp.Atom tag :: args) ->
    (match tag, args with
     | "Var", [ v ] -> Var ([%of_sexp: Tvar.t] v)
     | "Eq", [ a; b ] -> Eq (any_of_sexp a, any_of_sexp b)
     | "Ite", [ condition; then_; else_ ] ->
       Ite (any_of_sexp condition, any_of_sexp then_, any_of_sexp else_)
     | "Not", [ f ] -> Not (any_of_sexp f)
     | "And", [ fs ] ->
       And ([%of_sexp: Sexp.t list] fs |> List.map ~f:any_of_sexp)
     | "Or", [ fs ] -> Or ([%of_sexp: Sexp.t list] fs |> List.map ~f:any_of_sexp)
     | "App", [ f; args ] ->
       App
         ( [%of_sexp: Tvar.t] f
         , [%of_sexp: Sexp.t list] args |> List.map ~f:any_of_sexp )
     | "Select", [ array; index ] ->
       Select (any_of_sexp array, any_of_sexp index)
     | "Store", [ array; index; value ] ->
       Store (any_of_sexp array, any_of_sexp index, any_of_sexp value)
     | "Datatype_constructor", [ constructor; args ] ->
       Datatype_constructor
         ( [%of_sexp: Datatype.Constructor.t] constructor
         , [%of_sexp: Sexp.t list] args |> List.map ~f:any_of_sexp )
     | "Datatype_selector", [ selector; arg ] ->
       Datatype_selector
         ([%of_sexp: Datatype.Selector.t] selector, any_of_sexp arg)
     | "Datatype_tester", [ constructor; arg ] ->
       Datatype_tester
         ([%of_sexp: Datatype.Constructor.t] constructor, any_of_sexp arg)
     | "Function_type", [ a; b ] -> Function_type (any_of_sexp a, any_of_sexp b)
     | "Array_type", [ index; element ] ->
       Array_type (any_of_sexp index, any_of_sexp element)
     | "Type_of", [ f ] -> Type_of (any_of_sexp f)
     | "Type_var", [ v ] -> Type_var ([%of_sexp: Tvar.t] v)
     | "Type_app", [ f; args ] ->
       Type_app
         ( [%of_sexp: Tvar.t] f
         , [%of_sexp: Sexp.t list] args |> List.map ~f:any_of_sexp )
     | "La_const", [ q ] -> La_const ([%of_sexp: Q.t] q)
     | "La_scale_const", [ q; a ] ->
       La_scale_const ([%of_sexp: Q.t] q, any_of_sexp a)
     | "La_add", [ a; b ] -> La_add (any_of_sexp a, any_of_sexp b)
     | "La_compare", [ a; op; b ] ->
       La_compare
         (any_of_sexp a, [%of_sexp: [ `Le | `Ge | `Lt | `Gt ]] op, any_of_sexp b)
     | _ -> fail ())
  | Sexp.List _ -> fail ()
;;

let t_of_sexp (type a) (_a_of_sexp : Sexp.t -> a) (sexp : Sexp.t) : a t =
  (Obj.magic (any_of_sexp sexp : any) : a t)
;;

let rec quantified_of_sexp sexp : quantified =
  match sexp with
  | Sexp.List (Sexp.Atom "Forall" :: [ bound; triggers; body ]) ->
    Forall
      ( [%of_sexp: Tvar.t list] bound
      , [%of_sexp: Sexp.t list list] triggers
        |> List.map ~f:(List.map ~f:quantified_of_sexp)
      , quantified_of_sexp body )
  | Sexp.List [ Sexp.Atom "Exists"; bound; body ] ->
    Exists ([%of_sexp: Tvar.t list] bound, quantified_of_sexp body)
  | Sexp.List [ Sexp.Atom "Not"; f ] -> Not (quantified_of_sexp f)
  | Sexp.List [ Sexp.Atom "And"; fs ] ->
    And ([%of_sexp: Sexp.t list] fs |> List.map ~f:quantified_of_sexp)
  | Sexp.List [ Sexp.Atom "Or"; fs ] ->
    Or ([%of_sexp: Sexp.t list] fs |> List.map ~f:quantified_of_sexp)
  | _ -> widen_quantified (any_of_sexp sexp)
;;

let rank : type a. a t -> int = function
  | Var _ -> 0
  | Eq _ -> 1
  | Ite _ -> 2
  | True -> 3
  | False -> 4
  | Not _ -> 5
  | And _ -> 6
  | Or _ -> 7
  | Forall _ -> 8
  | Exists _ -> 9
  | App _ -> 10
  | Select _ -> 11
  | Store _ -> 12
  | Datatype_constructor _ -> 13
  | Datatype_selector _ -> 14
  | Datatype_tester _ -> 15
  | Bool -> 16
  | Int -> 17
  | Real -> 18
  | Int64 -> 19
  | Type -> 20
  | Function_type _ -> 21
  | Array_type _ -> 22
  | Type_of _ -> 23
  | Type_var _ -> 24
  | Type_app _ -> 25
  | La_const _ -> 26
  | La_scale_const _ -> 27
  | La_add _ -> 28
  | La_compare _ -> 29
;;

let lex first second = if first <> 0 then first else second ()

let rec compare_list_poly cmp l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | x1 :: rest1, x2 :: rest2 ->
    lex (cmp x1 x2) (fun () -> compare_list_poly cmp rest1 rest2)
;;

let rec compare_poly : type a b. a t -> b t -> int =
  fun t1 t2 ->
  match t1, t2 with
  | Var v1, Var v2 -> [%compare: Tvar.t] v1 v2
  | Eq (a1, b1), Eq (a2, b2) ->
    lex (compare_poly a1 a2) (fun () -> compare_poly b1 b2)
  | Ite (c1, t1, e1), Ite (c2, t2, e2) ->
    lex (compare_poly c1 c2) (fun () ->
      lex (compare_poly t1 t2) (fun () -> compare_poly e1 e2))
  | True, True -> 0
  | False, False -> 0
  | Not f1, Not f2 -> compare_poly f1 f2
  | And fs1, And fs2 -> compare_list_poly compare_poly fs1 fs2
  | Or fs1, Or fs2 -> compare_list_poly compare_poly fs1 fs2
  | Forall (b1, tr1, body1), Forall (b2, tr2, body2) ->
    lex
      ([%compare: Tvar.t list] b1 b2)
      (fun () ->
        lex
          (compare_list_poly (compare_list_poly compare_poly) tr1 tr2)
          (fun () -> compare_poly body1 body2))
  | Exists (b1, body1), Exists (b2, body2) ->
    lex ([%compare: Tvar.t list] b1 b2) (fun () -> compare_poly body1 body2)
  | App (f1, args1), App (f2, args2) ->
    lex
      ([%compare: Tvar.t] f1 f2)
      (fun () -> compare_list_poly compare_poly args1 args2)
  | Select (a1, i1), Select (a2, i2) ->
    lex (compare_poly a1 a2) (fun () -> compare_poly i1 i2)
  | Store (a1, i1, v1), Store (a2, i2, v2) ->
    lex (compare_poly a1 a2) (fun () ->
      lex (compare_poly i1 i2) (fun () -> compare_poly v1 v2))
  | Datatype_constructor (c1, args1), Datatype_constructor (c2, args2) ->
    lex
      ([%compare: Datatype.Constructor.t] c1 c2)
      (fun () -> compare_list_poly compare_poly args1 args2)
  | Datatype_selector (s1, a1), Datatype_selector (s2, a2) ->
    lex ([%compare: Datatype.Selector.t] s1 s2) (fun () -> compare_poly a1 a2)
  | Datatype_tester (c1, a1), Datatype_tester (c2, a2) ->
    lex
      ([%compare: Datatype.Constructor.t] c1 c2)
      (fun () -> compare_poly a1 a2)
  | Bool, Bool -> 0
  | Int, Int -> 0
  | Real, Real -> 0
  | Int64, Int64 -> 0
  | Type, Type -> 0
  | Function_type (a1, b1), Function_type (a2, b2) ->
    lex (compare_poly a1 a2) (fun () -> compare_poly b1 b2)
  | Array_type (i1, e1), Array_type (i2, e2) ->
    lex (compare_poly i1 i2) (fun () -> compare_poly e1 e2)
  | Type_of f1, Type_of f2 -> compare_poly f1 f2
  | Type_var v1, Type_var v2 -> [%compare: Tvar.t] v1 v2
  | Type_app (f1, args1), Type_app (f2, args2) ->
    lex
      ([%compare: Tvar.t] f1 f2)
      (fun () -> compare_list_poly compare_poly args1 args2)
  | La_const q1, La_const q2 -> [%compare: Q.t] q1 q2
  | La_scale_const (q1, a1), La_scale_const (q2, a2) ->
    lex ([%compare: Q.t] q1 q2) (fun () -> compare_poly a1 a2)
  | La_add (a1, b1), La_add (a2, b2) ->
    lex (compare_poly a1 a2) (fun () -> compare_poly b1 b2)
  | La_compare (a1, op1, b1), La_compare (a2, op2, b2) ->
    lex (compare_poly a1 a2) (fun () ->
      lex
        ([%compare: [ `Le | `Ge | `Lt | `Gt ]] op1 op2)
        (fun () -> compare_poly b1 b2))
  | Var _, _
  | Eq _, _
  | Ite _, _
  | True, _
  | False, _
  | Not _, _
  | And _, _
  | Or _, _
  | Forall _, _
  | Exists _, _
  | App _, _
  | Select _, _
  | Store _, _
  | Datatype_constructor _, _
  | Datatype_selector _, _
  | Datatype_tester _, _
  | Bool, _
  | Int, _
  | Real, _
  | Int64, _
  | Type, _
  | Function_type _, _
  | Array_type _, _
  | Type_of _, _
  | Type_var _, _
  | Type_app _, _
  | La_const _, _
  | La_scale_const _, _
  | La_add _, _
  | La_compare _, _ -> Int.compare (rank t1) (rank t2)
;;

let compare (type a) (_compare_a : a -> a -> int) (t1 : a t) (t2 : a t) : int =
  compare_poly t1 t2
;;

let equal (type a) (_equal_a : a -> a -> bool) (t1 : a t) (t2 : a t) : bool =
  compare_poly t1 t2 = 0
;;

let hash_fold_list_poly folder state list =
  List.fold list ~init:state ~f:(fun state x -> folder state x)
;;

let rec hash_fold_poly : type a. Hash.state -> a t -> Hash.state =
  fun state formula ->
  let state = Hash.fold_int state (rank formula) in
  match formula with
  | Var v -> [%hash_fold: Tvar.t] state v
  | Eq (a, b) -> hash_fold_poly (hash_fold_poly state a) b
  | Ite (condition, then_, else_) ->
    hash_fold_poly (hash_fold_poly (hash_fold_poly state condition) then_) else_
  | True -> state
  | False -> state
  | Bool -> state
  | Int -> state
  | Real -> state
  | Int64 -> state
  | Type -> state
  | Not f -> hash_fold_poly state f
  | And fs -> hash_fold_list_poly hash_fold_poly state fs
  | Or fs -> hash_fold_list_poly hash_fold_poly state fs
  | Forall (bound, triggers, body) ->
    let state = [%hash_fold: Tvar.t list] state bound in
    let state =
      List.fold triggers ~init:state ~f:(hash_fold_list_poly hash_fold_poly)
    in
    hash_fold_poly state body
  | Exists (bound, body) ->
    hash_fold_poly ([%hash_fold: Tvar.t list] state bound) body
  | App (f, args) ->
    hash_fold_list_poly hash_fold_poly ([%hash_fold: Tvar.t] state f) args
  | Select (array, index) -> hash_fold_poly (hash_fold_poly state array) index
  | Store (array, index, value) ->
    hash_fold_poly (hash_fold_poly (hash_fold_poly state array) index) value
  | Datatype_constructor (constructor, args) ->
    hash_fold_list_poly
      hash_fold_poly
      ([%hash_fold: Datatype.Constructor.t] state constructor)
      args
  | Datatype_selector (selector, arg) ->
    hash_fold_poly ([%hash_fold: Datatype.Selector.t] state selector) arg
  | Datatype_tester (constructor, arg) ->
    hash_fold_poly ([%hash_fold: Datatype.Constructor.t] state constructor) arg
  | Function_type (a, b) -> hash_fold_poly (hash_fold_poly state a) b
  | Array_type (index, element) ->
    hash_fold_poly (hash_fold_poly state index) element
  | Type_of f -> hash_fold_poly state f
  | Type_var v -> [%hash_fold: Tvar.t] state v
  | Type_app (f, args) ->
    hash_fold_list_poly hash_fold_poly ([%hash_fold: Tvar.t] state f) args
  | La_const q -> [%hash_fold: Q.t] state q
  | La_scale_const (q, a) -> hash_fold_poly ([%hash_fold: Q.t] state q) a
  | La_add (a, b) -> hash_fold_poly (hash_fold_poly state a) b
  | La_compare (a, op, b) ->
    hash_fold_poly
      ([%hash_fold: [ `Le | `Ge | `Lt | `Gt ]] (hash_fold_poly state a) op)
      b
;;

let hash_fold_t
  (type a)
  (_hash_fold_a : Hash.state -> a -> Hash.state)
  (state : Hash.state)
  (t : a t)
  : Hash.state
  =
  hash_fold_poly state t
;;

let sexp_of_any (a : any) : Sexp.t = sexp_of_t_any a
let compare_any (a : any) (b : any) : int = compare_poly a b
let equal_any (a : any) (b : any) : bool = compare_poly a b = 0

let hash_fold_any (state : Hash.state) (a : any) : Hash.state =
  hash_fold_poly state a
;;

let hash_any (a : any) : int = Hash.run hash_fold_poly a

module Any = struct
  type t = any [@@deriving sexp, compare, hash]

  include functor Comparable.Make
  include functor Hashable.Make
end

let compare_quantified (a : quantified) (b : quantified) : int =
  compare_poly a b
;;

let equal_quantified (a : quantified) (b : quantified) : bool =
  compare_poly a b = 0
;;

let hash_fold_quantified (state : Hash.state) (a : quantified) : Hash.state =
  hash_fold_poly state a
;;

let hash_quantified (a : quantified) : int = Hash.run hash_fold_poly a

module Quantified = struct
  type t = quantified [@@deriving sexp_of, compare, hash]

  include functor Comparable.Make_plain
  include functor Hashable.Make_plain
end

let rec tvars_fold (acc : Tvar.Set.t) (q : quantified) : Tvar.Set.t =
  let acc =
    match op q with
    | Var v | App v | Type_var v | Type_app v -> Set.add acc v
    | Forall bound | Exists bound -> List.fold bound ~init:acc ~f:Set.add
    | _ -> acc
  in
  List.fold (quantified_args q) ~init:acc ~f:tvars_fold
;;

let tvars (q : quantified) : Tvar.Set.t = tvars_fold Tvar.Set.empty q
