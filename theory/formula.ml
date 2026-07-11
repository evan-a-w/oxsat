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

(* Inverse of [sexp_of_t]. Can't be typed [type a. Sexp.t -> a t] since e.g.
   [Bool]/[La_const] can only produce specific (open) tags, not an arbitrary [a]
   -- so this is used monomorphically at each concrete instantiation (there's
   exactly one call site, {!Uf.Term.t_of_sexp}). *)
let rec t_of_sexp sexp =
  let fail () = of_sexp_error "Formula.t_of_sexp: unexpected sexp" sexp in
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
     | "Eq", [ a; b ] -> Eq (t_of_sexp a, t_of_sexp b)
     | "Not", [ f ] -> Not (t_of_sexp f)
     | "And", [ fs ] -> And ([%of_sexp: Sexp.t list] fs |> List.map ~f:t_of_sexp)
     | "Or", [ fs ] -> Or ([%of_sexp: Sexp.t list] fs |> List.map ~f:t_of_sexp)
     | "App", [ f; args ] ->
       App
         ( [%of_sexp: Tvar.t] f
         , [%of_sexp: Sexp.t list] args |> List.map ~f:t_of_sexp )
     | "Function_type", [ a; b ] -> Function_type (t_of_sexp a, t_of_sexp b)
     | "Type_of", [ f ] -> Type_of (t_of_sexp f)
     | "Type_app", [ f; a ] -> Type_app ([%of_sexp: Tvar.t] f, t_of_sexp a)
     | "La_const", [ q ] -> La_const ([%of_sexp: Q.t] q)
     | "La_scale_const", [ q; a ] ->
       La_scale_const ([%of_sexp: Q.t] q, t_of_sexp a)
     | "La_add", [ a; b ] -> La_add (t_of_sexp a, t_of_sexp b)
     | "La_compare", [ a; op; b ] ->
       La_compare
         (t_of_sexp a, [%of_sexp: [ `Le | `Ge | `Lt | `Gt ]] op, t_of_sexp b)
     | _ -> fail ())
  | Sexp.List _ -> fail ()
;;

(* [ppx_compare]/[ppx_hash] can't derive these for GADTs either, so -- like
   [sexp_of_t] -- they're written by hand. Each constructor gets a fixed rank;
   nodes with different ranks compare by rank, nodes with the same rank compare
   structurally on their payload. *)
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

(* [lex] runs [second] only if [first] was [0], without forcing its two
   arguments to share a type (needed since [compare_poly] compares across
   possibly-different phantom tags). *)
let lex first second = if first <> 0 then first else second ()

(* Like [List.compare], but doesn't force the two lists' element types to be the
   same (needed since [compare_poly] compares across possibly-different phantom
   tags). *)
let rec compare_list_poly cmp l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | x1 :: rest1, x2 :: rest2 ->
    lex (cmp x1 x2) (fun () -> compare_list_poly cmp rest1 rest2)
;;

(* Rank-2 polymorphic so that constructors whose payload's tag is unconstrained
   by the result (e.g. [Type_of]'s argument) can still be compared recursively;
   the phantom tag never affects the runtime representation, so comparing across
   two different instantiations is always sound. *)
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

let compare (type a) (t1 : a t) (t2 : a t) = compare_poly t1 t2
let equal (type a) (t1 : a t) (t2 : a t) = compare_poly t1 t2 = 0

(* Like [List.fold], but the folding function is
   [type a. Hash.state -> a t -> Hash.state] (rank-2 polymorphic in the same way
   [compare_poly] is), so a single [hash_fold_t] can be used to fold every
   element regardless of its phantom tag. *)
let hash_fold_list_poly folder state list =
  List.fold list ~init:state ~f:(fun state x -> folder state x)
;;

(* Rank-2 polymorphic for the same reason [compare_poly] is: e.g. [Type_of]'s
   argument has a tag unconstrained by the result. Each constructor first folds
   in its [rank] (so that e.g. [True] and [Bool] -- both zero-argument -- hash
   differently), then its payload. *)
let rec hash_fold_t : type a. Hash.state -> a t -> Hash.state =
  fun state formula ->
  let state = Hash.fold_int state (rank formula) in
  match formula with
  | Var v -> [%hash_fold: Tvar.t] state v
  | Eq (a, b) -> hash_fold_t (hash_fold_t state a) b
  | True -> state
  | False -> state
  | Bool -> state
  | Int -> state
  | Float -> state
  | Type -> state
  | Not f -> hash_fold_t state f
  | And fs -> hash_fold_list_poly hash_fold_t state fs
  | Or fs -> hash_fold_list_poly hash_fold_t state fs
  | App (f, args) ->
    hash_fold_list_poly hash_fold_t ([%hash_fold: Tvar.t] state f) args
  | Function_type (a, b) -> hash_fold_t (hash_fold_t state a) b
  | Type_of f -> hash_fold_t state f
  | Type_app (f, a) -> hash_fold_t ([%hash_fold: Tvar.t] state f) a
  | La_const q -> [%hash_fold: Q.t] state q
  | La_scale_const (q, a) -> hash_fold_t ([%hash_fold: Q.t] state q) a
  | La_add (a, b) -> hash_fold_t (hash_fold_t state a) b
  | La_compare (a, op, b) ->
    hash_fold_t
      ([%hash_fold: [ `Le | `Ge | `Lt | `Gt ]] (hash_fold_t state a) op)
      b
;;

let hash (type a) (t : a t) = Hash.run hash_fold_t t

type any = [ `Boolean | `Uf | `Type | `La ] t

let sexp_of_any = sexp_of_t
let any_of_sexp sexp : any = t_of_sexp sexp
let compare_any = compare
let equal_any = equal
let hash_any = hash
let hash_fold_any = hash_fold_t

module Uf = Uninterpreted_functions.Make (struct
    type nonrec t = [ `Uf ] t

    let sexp_of_t = sexp_of_t
    let compare = compare
    let hash = hash
    let hash_fold_t = hash_fold_t

    (* Only [Var]/[App] shapes are valid here (see [split_function] below), so
       this is a dedicated parser rather than a narrowing of the general
       [Formula.t_of_sexp] (whose result type is necessarily open, since most
       constructors can only produce specific tags). *)
    let rec t_of_sexp (sexp : Sexp.t) : t =
      match sexp with
      | List [ Atom "Var"; v ] -> Var ([%of_sexp: Tvar.t] v)
      | List [ Atom "App"; f; args ] ->
        App
          ( [%of_sexp: Tvar.t] f
          , [%of_sexp: Sexp.t list] args |> List.map ~f:t_of_sexp )
      | _ -> of_sexp_error "Formula.Uf.Term.t_of_sexp: expected Var or App" sexp
    ;;

    (* [Eq]/[Not]/[And]/... can all type-check at [`Uf] (their result type is
       polymorphic in the tag), but by convention only [Var] and [App] are ever
       actually constructed here -- everything that reaches [Formula.Uf] has
       already been validated as a genuine UF term (see [Encoding.uf_term_of]). *)
    let split_function : t -> (Tvar.t * t list) option = function
      | Var _ -> None
      | App (function_, args) -> Some (function_, args)
      | _ -> assert false
    ;;

    let garbage_for_vec : t = Var (Tvar.of_string "")

    include functor Comparable.Make
    include functor Hashable.Make
  end)
