open! Core
open! Feel.Import

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

let op t : Op.t =
  match t with
  | Var v -> Var v
  | Eq _ -> Eq
  | True -> True
  | False -> False
  | Not _ -> Not
  | And _ -> And
  | Or _ -> Or
  | App (v, _) -> App v
  | Bool -> Bool
  | Int -> Int
  | Float -> Float
  | Type -> Type
  | Function_type _ -> Function_type
  | Type_of _ -> Type_of
  | Type_app (v, _) -> Type_app v
  | La_const q -> La_const q
  | La_scale_const (q, _) -> La_scale_const q
  | La_add _ -> La_add
  | La_compare (_, op, _) -> La_compare op
;;

let args t : _ t list =
  match t with
  | Var _ -> []
  | Eq (a, b) -> [ a; b ]
  | True -> []
  | False -> []
  | Not x -> [ x ]
  | And l -> l
  | Or l -> l
  | App (_, l) -> l
  | Bool -> []
  | Int -> []
  | Float -> []
  | Type -> []
  | Function_type (a, b) -> [ a; b ]
  | Type_of x -> [ x ]
  | Type_app (_, l) -> [ l ]
  | La_const _ -> []
  | La_scale_const (_, r) -> [ r ]
  | La_add (a, b) -> [ a; b ]
  | La_compare (a, _, b) -> [ a; b ]
;;

type any = [ `Boolean | `Uf | `Type | `La | `Term | `Atom ] t

let rec sexp_of_t : type a. (a -> Sexp.t) -> a t -> Sexp.t =
  fun sexp_of_a formula ->
  let node tag args = Sexp.List (Sexp.Atom tag :: args) in
  let sexp_of_t a = sexp_of_t sexp_of_a a in
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

let sexp_of_t_any : type a. a t -> Sexp.t =
  fun f -> sexp_of_t (fun _ -> assert false) f
;;

let rec any_of_sexp sexp : any =
  let fail () = of_sexp_error "Formula.any_of_sexp: unexpected sexp" sexp in
  match sexp with
  | Sexp.Atom "True" -> True
  | Sexp.Atom "False" -> False
  | Sexp.Atom "Bool" -> Bool
  | Sexp.Atom "Int" -> Int
  | Sexp.Atom "Float" -> Float
  | Sexp.Atom "Type" -> Type
  | Sexp.Atom _ -> fail ()
  | Sexp.List (Sexp.Atom tag :: args) ->
    (match tag, args with
     | "Var", [ v ] -> Var ([%of_sexp: Tvar.t] v)
     | "Eq", [ a; b ] -> Eq (any_of_sexp a, any_of_sexp b)
     | "Not", [ f ] -> Not (any_of_sexp f)
     | "And", [ fs ] ->
       And ([%of_sexp: Sexp.t list] fs |> List.map ~f:any_of_sexp)
     | "Or", [ fs ] -> Or ([%of_sexp: Sexp.t list] fs |> List.map ~f:any_of_sexp)
     | "App", [ f; args ] ->
       App
         ( [%of_sexp: Tvar.t] f
         , [%of_sexp: Sexp.t list] args |> List.map ~f:any_of_sexp )
     | "Function_type", [ a; b ] -> Function_type (any_of_sexp a, any_of_sexp b)
     | "Type_of", [ f ] -> Type_of (any_of_sexp f)
     | "Type_app", [ f; a ] -> Type_app ([%of_sexp: Tvar.t] f, any_of_sexp a)
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

let rank : type a. a t -> int = function
  | Var _ -> 0
  | Eq _ -> 1
  | True -> 2
  | False -> 3
  | Not _ -> 4
  | And _ -> 5
  | Or _ -> 6
  | App _ -> 7
  | Bool -> 8
  | Int -> 9
  | Float -> 10
  | Type -> 11
  | Function_type _ -> 12
  | Type_of _ -> 13
  | Type_app _ -> 14
  | La_const _ -> 15
  | La_scale_const _ -> 16
  | La_add _ -> 17
  | La_compare _ -> 18
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
  | True, True -> 0
  | False, False -> 0
  | Not f1, Not f2 -> compare_poly f1 f2
  | And fs1, And fs2 -> compare_list_poly compare_poly fs1 fs2
  | Or fs1, Or fs2 -> compare_list_poly compare_poly fs1 fs2
  | App (f1, args1), App (f2, args2) ->
    lex
      ([%compare: Tvar.t] f1 f2)
      (fun () -> compare_list_poly compare_poly args1 args2)
  | Bool, Bool -> 0
  | Int, Int -> 0
  | Float, Float -> 0
  | Type, Type -> 0
  | Function_type (a1, b1), Function_type (a2, b2) ->
    lex (compare_poly a1 a2) (fun () -> compare_poly b1 b2)
  | Type_of f1, Type_of f2 -> compare_poly f1 f2
  | Type_app (f1, a1), Type_app (f2, a2) ->
    lex ([%compare: Tvar.t] f1 f2) (fun () -> compare_poly a1 a2)
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
  | True, _
  | False, _
  | Not _, _
  | And _, _
  | Or _, _
  | App _, _
  | Bool, _
  | Int, _
  | Float, _
  | Type, _
  | Function_type _, _
  | Type_of _, _
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
  | True -> state
  | False -> state
  | Bool -> state
  | Int -> state
  | Float -> state
  | Type -> state
  | Not f -> hash_fold_poly state f
  | And fs -> hash_fold_list_poly hash_fold_poly state fs
  | Or fs -> hash_fold_list_poly hash_fold_poly state fs
  | App (f, args) ->
    hash_fold_list_poly hash_fold_poly ([%hash_fold: Tvar.t] state f) args
  | Function_type (a, b) -> hash_fold_poly (hash_fold_poly state a) b
  | Type_of f -> hash_fold_poly state f
  | Type_app (f, a) -> hash_fold_poly ([%hash_fold: Tvar.t] state f) a
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

module Uf = Uninterpreted_functions.Make (struct
    type nonrec t = [ `Uf | `Term ] t [@@deriving sexp_of, compare, hash]

    let rec t_of_sexp (sexp : Sexp.t) : t =
      match sexp with
      | List [ Atom "Var"; v ] -> Var ([%of_sexp: Tvar.t] v)
      | List [ Atom "App"; f; args ] ->
        App
          ( [%of_sexp: Tvar.t] f
          , [%of_sexp: Sexp.t list] args |> List.map ~f:t_of_sexp )
      | _ -> of_sexp_error "Formula.Uf.Term.t_of_sexp: expected Var or App" sexp
    ;;

    let split_function : t -> (Tvar.t * t list) option = function
      | Var _ -> None
      | App (function_, args) -> Some (function_, args)
      | _ -> .
    ;;

    let garbage_for_vec : t = Var (Tvar.of_string "")

    include functor Comparable.Make
    include functor Hashable.Make
  end)
