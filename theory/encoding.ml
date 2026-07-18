open! Core
open! Feel.Import

module Formula_with_no_shared_theories = struct
  type t =
    | True
    | False
    | Atom of Atom.t
    | Not of t
    | And of t list
    | Or of t list
  [@@deriving sexp_of]
end

type t =
  { mutable next_var : int
  ; atom_to_sat_var : int Atom.Table.t
  ; sat_var_to_atom : Atom.t Int.Table.t
  ; atoms_in_order : (Atom.t * int) Vec.Value.t
  ; mutable true_var : int or_null
  ; theory_for_tvar : Formula.Theory.Packed.t Tvar.Table.t
  ; newly_shared : Tvar.t Vec.Value.t
  }

let create () =
  { next_var = 1
  ; atom_to_sat_var = Atom.Table.create ()
  ; sat_var_to_atom = Int.Table.create ()
  ; atoms_in_order = Vec.Value.create ()
  ; true_var = Null
  ; theory_for_tvar = Tvar.Table.create ()
  ; newly_shared = Vec.Value.create ()
  }
;;

let record_tvar t tvar theory =
  let shared = Formula.Theory.(Packed.T Shared) in
  Hashtbl.update t.theory_for_tvar tvar ~f:(fun existing ->
    let updated =
      match existing with
      | None -> theory
      | Some existing -> Formula.Theory.Packed.join existing theory
    in
    let was_shared =
      match existing with
      | None -> false
      | Some existing -> Formula.Theory.Packed.equal existing shared
    in
    if (not was_shared) && Formula.Theory.Packed.equal updated shared
    then Vec.Value.push t.newly_shared tvar;
    updated)
;;

let drain_newly_shared t =
  let tvars = Vec.Value.to_list t.newly_shared in
  Vec.Value.clear t.newly_shared;
  tvars
;;

let rec record_uf_term_tvars t (formula : Formula.any) =
  match formula with
  | Var v -> record_tvar t v Formula.Theory.(Packed.T Uf)
  | App (f, args) ->
    record_tvar t f Formula.Theory.(Packed.T Uf);
    List.iter args ~f:(record_uf_term_tvars t)
  | _ -> ()
;;

let rec record_type_expr_tvars t (type_expr : Type_expr.t) =
  match type_expr with
  | Var v | Type_of v -> record_tvar t v Formula.Theory.(Packed.T Type)
  | Base _ | Type -> ()
  | Function_type (a, b) ->
    record_type_expr_tvars t a;
    record_type_expr_tvars t b
  | App (f, args) ->
    record_tvar t f Formula.Theory.(Packed.T Type);
    List.iter args ~f:(record_type_expr_tvars t)
;;

(* A bare [Eq (Var _, Var _)] is the theory-agnostic bridge equality: it doesn't
   by itself place its endpoints in any theory. *)
let record_atom_tvars t (atom : Atom.t) =
  match atom with
  | `Eq (Var _, Var _) -> ()
  | `Eq (a, b) ->
    record_uf_term_tvars t a;
    record_uf_term_tvars t b
  | `Type_eq (a, b) ->
    record_type_expr_tvars t a;
    record_type_expr_tvars t b
  | `Le (le, _) ->
    Map.iter_keys le.coeffs ~f:(fun v ->
      record_tvar t v Formula.Theory.(Packed.T La))
;;

let theory_for_tvar t tvar = Hashtbl.find t.theory_for_tvar tvar

let tvar_theories t =
  Hashtbl.to_alist t.theory_for_tvar |> Tvar.Map.of_alist_exn
;;

let fresh_var t =
  let var = t.next_var in
  t.next_var <- t.next_var + 1;
  var
;;

let sat_var_for_atom t atom =
  let atom = Atom.normalize atom in
  Hashtbl.find_or_add t.atom_to_sat_var atom ~default:(fun () ->
    record_atom_tvars t atom;
    let sat_var = fresh_var t in
    Hashtbl.set t.sat_var_to_atom ~key:sat_var ~data:atom;
    Vec.Value.push t.atoms_in_order (atom, sat_var);
    sat_var)
;;

let find_sat_var_for_atom t atom =
  Hashtbl.find t.atom_to_sat_var (Atom.normalize atom)
;;

let atom_for_sat_var t sat_var = Hashtbl.find t.sat_var_to_atom sat_var
let atoms t = Vec.Value.to_list t.atoms_in_order

(* A variable that is always true: a fresh variable with a unit clause asserting
   it, allocated lazily and cached so [True]/[False] don't waste a variable when
   unused. *)
let true_literal t ~clauses =
  match t.true_var with
  | This var -> var
  | Null ->
    let var = fresh_var t in
    Vec.Value.push clauses [| var |];
    t.true_var <- This var;
    var
;;

(* Returns a literal representing [formula]'s truth value, pushing any Tseitin
   definition clauses for subformulas onto [clauses]. *)
let rec literal_of t ~clauses ~(formula : Formula_with_no_shared_theories.t)
  : int
  =
  match formula with
  | True -> true_literal t ~clauses
  | False -> -true_literal t ~clauses
  | Atom atom -> sat_var_for_atom t atom
  | Not formula -> -literal_of t ~clauses ~formula
  | And fs ->
    let literals =
      List.map fs ~f:(fun formula -> literal_of t ~clauses ~formula)
    in
    let d = fresh_var t in
    (* d -> l_i, for each i *)
    List.iter literals ~f:(fun l -> Vec.Value.push clauses [| -d; l |]);
    (* (l_1 /\ ... /\ l_n) -> d *)
    Vec.Value.push clauses (Array.of_list (d :: List.map literals ~f:( ~- )));
    d
  | Or fs ->
    let literals =
      List.map fs ~f:(fun formula -> literal_of t ~clauses ~formula)
    in
    let d = fresh_var t in
    (* l_i -> d, for each i *)
    List.iter literals ~f:(fun l -> Vec.Value.push clauses [| -l; d |]);
    (* d -> (l_1 \/ ... \/ l_n) *)
    Vec.Value.push clauses (Array.of_list (-d :: literals));
    d
;;

(* [Formula.t]'s type parameter is a phantom tag, erased at runtime, so we can't
   dispatch on it directly; instead we classify a node by its outermost
   constructor, which unambiguously tells us which theory (if any) it belongs to
   -- except for [Var] and [Eq], which are shared by every theory. *)
module Shape = struct
  type t =
    | Bool
    | Uf
    | Type
    | La
    | Var of Tvar.t
end

let shape_of (type a) (formula : a Formula.t) : Shape.t =
  match formula with
  | Var v -> Var v
  | Eq (_, _) ->
    Bool
    (* unreachable: [shape_of] is only ever called on [Eq]'s own arguments *)
  | True | False | Not _ | And _ | Or _ -> Bool
  | App (_, _) -> Uf
  | Bool | Int | Float | Type
  | Function_type (_, _)
  | Type_of _ | Type_var _
  | Type_app (_, _) -> Type
  | La_const _ | La_scale_const (_, _) | La_add (_, _) | La_compare (_, _, _) ->
    La
;;

let rec uf_term_of : type a. a Formula.t -> Formula.any Or_error.t =
  fun formula ->
  match formula with
  | Var v -> Ok (Formula.Var v)
  | App (function_, args) ->
    let%bind.Or_error args = Or_error.all (List.map args ~f:uf_term_of) in
    Ok (Formula.App (function_, args))
  | _ -> Or_error.error_s [%message "formula is not a UF term"]
;;

let rec type_expr_of : type a. a Formula.t -> Type_expr.t Or_error.t =
  fun formula ->
  match formula with
  | Var v -> Ok (Type_expr.Var v)
  | Type_var v -> Ok (Type_expr.Var v)
  | Bool -> Ok (Type_expr.Base Bool)
  | Int -> Ok (Type_expr.Base Int)
  | Float -> Ok (Type_expr.Base Float)
  | Type -> Ok Type_expr.Type
  | Function_type (a, b) ->
    let%bind.Or_error a = type_expr_of a in
    let%bind.Or_error b = type_expr_of b in
    Ok (Type_expr.Function_type (a, b))
  | Type_of a ->
    (match a with
     | Var v -> Ok (Type_expr.Type_of v)
     | _ ->
       Or_error.error_s
         [%message "Type_of is only supported applied to a variable"])
  | Type_app (f, args) ->
    let%bind.Or_error args = args |> List.map ~f:type_expr_of |> Or_error.all in
    Ok (Type_expr.App (f, args))
  | _ -> Or_error.error_s [%message "formula is not a type expression"]
;;

let rec linear_expr_of : type a. a Formula.t -> Linear_expr.t Or_error.t =
  fun formula ->
  match formula with
  | Var v -> Ok (Linear_expr.var v)
  | La_const q -> Ok (Linear_expr.const q)
  | La_scale_const (q, a) ->
    let%bind.Or_error a = linear_expr_of a in
    Ok (Linear_expr.scale q a)
  | La_add (a, b) ->
    let%bind.Or_error a = linear_expr_of a in
    let%bind.Or_error b = linear_expr_of b in
    Ok Linear_expr.(a + b)
  | _ -> Or_error.error_s [%message "formula is not a linear expression"]
;;

let le_atoms_of_eq (a : Linear_expr.t) (b : Linear_expr.t) : Atom.t list =
  let diff = Linear_expr.(a - b) in
  [ `Le (diff, Q.zero); `Le (Linear_expr.neg diff, Q.zero) ]
;;

let compare_atom
  (a : Linear_expr.t)
  (op : [ `Le | `Ge | `Lt | `Gt ])
  (b : Linear_expr.t)
  : Formula_with_no_shared_theories.t
  =
  match op with
  | `Le -> Atom (`Le (Linear_expr.(a - b), Q.zero))
  | `Ge -> Atom (`Le (Linear_expr.(b - a), Q.zero))
  | `Lt -> Not (Atom (`Le (Linear_expr.(b - a), Q.zero)))
  | `Gt -> Not (Atom (`Le (Linear_expr.(a - b), Q.zero)))
;;

let rec bool_formula_of
  : type a. a Formula.t -> Formula_with_no_shared_theories.t Or_error.t
  =
  fun formula ->
  let module F = Formula_with_no_shared_theories in
  match formula with
  | True -> Ok F.True
  | False -> Ok F.False
  | Not (Eq (a, b)) -> neq_formula_of a b
  | Not f ->
    let%bind.Or_error f = bool_formula_of f in
    Ok (F.Not f)
  | And fs ->
    let%bind.Or_error fs = Or_error.all (List.map fs ~f:bool_formula_of) in
    Ok (F.And fs)
  | Or fs ->
    let%bind.Or_error fs = Or_error.all (List.map fs ~f:bool_formula_of) in
    Ok (F.Or fs)
  | La_compare (a, op, b) ->
    let%bind.Or_error a = linear_expr_of a in
    let%bind.Or_error b = linear_expr_of b in
    Ok (compare_atom a op b)
  | Eq (a, b) -> eq_formula_of a b
  | Var _ -> Or_error.error_s [%message "a bare variable is not a formula"]
  | _ -> Or_error.error_s [%message "formula is not boolean"]

and eq_formula_of
  : type a.
    a Formula.t -> a Formula.t -> Formula_with_no_shared_theories.t Or_error.t
  =
  fun a b ->
  let module F = Formula_with_no_shared_theories in
  match shape_of a, shape_of b with
  | Bool, _ | _, Bool ->
    let%bind.Or_error a = bool_formula_of a in
    let%bind.Or_error b = bool_formula_of b in
    (* [a <-> b] as [(¬a \/ b) /\ (a \/ ¬b)] *)
    Ok (F.And [ F.Or [ F.Not a; b ]; F.Or [ a; F.Not b ] ])
  | Type, _ | _, Type ->
    let%bind.Or_error a = type_expr_of a in
    let%bind.Or_error b = type_expr_of b in
    Ok (F.Atom (`Type_eq (a, b)))
  | La, _ | _, La ->
    let%bind.Or_error a = linear_expr_of a in
    let%bind.Or_error b = linear_expr_of b in
    Ok (F.And (List.map (le_atoms_of_eq a b) ~f:(fun atom -> F.Atom atom)))
  | Uf, _ | _, Uf ->
    let%bind.Or_error a = uf_term_of a in
    let%bind.Or_error b = uf_term_of b in
    Ok (F.Atom (`Eq (a, b)))
  | Var _, Var _ ->
    let%bind.Or_error uf_a = uf_term_of a in
    let%bind.Or_error uf_b = uf_term_of b in
    Ok (F.Atom (`Eq (uf_a, uf_b)))

(* Mirrors [eq_formula_of], negating each theory's encoding. A bare-variable
   disequality is just the negated bridge equality: numeric disequality is
   injected lazily by [Bare_var_eq] once both endpoints are known numeric, and
   types are left unconstrained (distinct values may share a type). *)
and neq_formula_of
  : type a.
    a Formula.t -> a Formula.t -> Formula_with_no_shared_theories.t Or_error.t
  =
  fun a b ->
  let module F = Formula_with_no_shared_theories in
  match shape_of a, shape_of b with
  | Bool, _ | _, Bool ->
    let%bind.Or_error eq = eq_formula_of a b in
    Ok (F.Not eq)
  | Type, _ | _, Type ->
    let%bind.Or_error a = type_expr_of a in
    let%bind.Or_error b = type_expr_of b in
    Ok (F.Not (F.Atom (`Type_eq (a, b))))
  | La, _ | _, La ->
    let%bind.Or_error a = linear_expr_of a in
    let%bind.Or_error b = linear_expr_of b in
    Ok
      (F.Not
         (F.And (List.map (le_atoms_of_eq a b) ~f:(fun atom -> F.Atom atom))))
  | Uf, _ | _, Uf ->
    let%bind.Or_error a = uf_term_of a in
    let%bind.Or_error b = uf_term_of b in
    Ok (F.Not (F.Atom (`Eq (a, b))))
  | Var _, Var _ ->
    let%bind.Or_error uf_a = uf_term_of a in
    let%bind.Or_error uf_b = uf_term_of b in
    Ok (F.Not (F.Atom (`Eq (uf_a, uf_b))))
;;

let rec type_expr_to_formula : Type_expr.t -> [> `Type ] Formula.t = function
  | Var v -> Type_var v
  | Base Bool -> Bool
  | Base Int -> Int
  | Base Float -> Float
  | Type_of v -> Type_of (Var v)
  | App (f, args) -> Type_app (f, List.map args ~f:type_expr_to_formula)
  | Type_expr.Type -> Formula.Type
  | Function_type (a, b) ->
    Function_type (type_expr_to_formula a, type_expr_to_formula b)
;;

let linear_expr_to_formula (le : Linear_expr.t) : [> `La | `Term ] Formula.t =
  let term_summands =
    Map.to_alist le.coeffs
    |> List.map ~f:(fun (v, q) -> Formula.La_scale_const (q, Var v))
  in
  let summands =
    if Q.is_zero le.const
    then term_summands
    else Formula.La_const le.const :: term_summands
  in
  match summands with
  | [] -> Formula.La_const Q.zero
  | first :: rest ->
    List.fold rest ~init:first ~f:(fun acc s -> Formula.La_add (acc, s))
;;

let atom_to_formula : Atom.t -> Formula.any = function
  | `Eq (a, b) -> Eq (a, b)
  | `Type_eq (a, b) -> Eq (type_expr_to_formula a, type_expr_to_formula b)
  | `Le (le, c) -> La_compare (linear_expr_to_formula le, `Le, La_const c)
;;

let encode (encoding : t) ~(formula : [> `Boolean ] Formula.t)
  : int array list Or_error.t
  =
  let%bind.Or_error formula = bool_formula_of formula in
  let clauses = Vec.Value.create () in
  let root = literal_of encoding ~clauses ~formula in
  Vec.Value.push clauses [| root |];
  Ok (Vec.Value.to_list clauses)
;;
