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
  [@@deriving sexp]
end

type t =
  { mutable next_var : int
  ; mutable next_tvar : int
  ; atom_to_sat_var : int Atom.Table.t
  ; sat_var_to_atom : Atom.t Int.Table.t
  ; atoms_in_order : (Atom.t * int) Vec.Value.t
  ; shared_tvars_in_order : Tvar.t Vec.Value.t
  ; mutable true_var : int or_null
  ; atom_to_shared_tvar : Tvar.t Atom.Table.t
  }
[@@warning "-69"]

module Checkpoint = struct
  type t =
    { atoms_length : int
    ; shared_tvars_length : int
    }
end

let create () =
  { next_var = 1
  ; next_tvar = 1
  ; atom_to_sat_var = Atom.Table.create ()
  ; sat_var_to_atom = Int.Table.create ()
  ; atoms_in_order = Vec.Value.create ()
  ; true_var = Null
  ; atom_to_shared_tvar = Atom.Table.create ()
  ; shared_tvars_in_order = Vec.Value.create ()
  }
;;

let fresh_var t =
  let var = t.next_var in
  t.next_var <- t.next_var + 1;
  var
;;

let (_fresh_tvar : t -> Tvar.t) =
  fun t ->
  let raw = t.next_tvar in
  t.next_tvar <- t.next_tvar + 1;
  let s = [%string "temp_var#%{raw#Int}"] in
  Tvar.of_string s
;;

let sat_var_for_atom t atom =
  let atom = Atom.normalize atom in
  Hashtbl.find_or_add t.atom_to_sat_var atom ~default:(fun () ->
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

let checkpoint t : Checkpoint.t =
  { atoms_length = Vec.Value.length t.atoms_in_order
  ; shared_tvars_length = Vec.Value.length t.shared_tvars_in_order
  }
;;

let new_atoms_since t ~checkpoint:({ atoms_length; _ } : Checkpoint.t) =
  List.init
    (Vec.Value.length t.atoms_in_order - atoms_length)
    ~f:(fun i -> Vec.Value.get t.atoms_in_order (atoms_length + i))
;;

let new_shared_tvars_since
  t
  ~checkpoint:({ shared_tvars_length; _ } : Checkpoint.t)
  =
  List.init
    (Vec.Value.length t.shared_tvars_in_order - shared_tvars_length)
    ~f:(fun i ->
      Vec.Value.get t.shared_tvars_in_order (shared_tvars_length + i))
;;

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
  | Type_of _
  | Type_app (_, _) -> Type
  | La_const _ | La_scale_const (_, _) | La_add (_, _) | La_compare (_, _, _) ->
    La
;;

let rec uf_term_of : type a. a Formula.t -> Uf_term.t Or_error.t =
  fun formula ->
  match formula with
  | Var v -> Ok (`Var v)
  | App (function_, args) ->
    let%bind.Or_error args = Or_error.all (List.map args ~f:uf_term_of) in
    Ok (`App (~function_, ~args))
  | _ -> Or_error.error_s [%message "formula is not a UF term"]
;;

let rec type_expr_of : type a. a Formula.t -> Type_expr.t Or_error.t =
  fun formula ->
  match formula with
  | Var v -> Ok (Type_expr.Var v)
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
  | Type_app (f, a) ->
    let%bind.Or_error a = type_expr_of a in
    Ok (Type_expr.App (f, [ a ]))
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
    (* Fully ambiguous: propagate the equality into every theory that shares
       [Tvar.t] as a namespace. *)
    let%bind.Or_error uf_a = uf_term_of a in
    let%bind.Or_error uf_b = uf_term_of b in
    let%bind.Or_error type_a = type_expr_of a in
    let%bind.Or_error type_b = type_expr_of b in
    let%bind.Or_error la_a = linear_expr_of a in
    let%bind.Or_error la_b = linear_expr_of b in
    Ok
      (F.And
         [ F.Atom (`Eq (uf_a, uf_b))
         ; F.Atom (`Type_eq (type_a, type_b))
         ; F.And
             (List.map (le_atoms_of_eq la_a la_b) ~f:(fun atom -> F.Atom atom))
         ])

(* Mirrors [eq_formula_of], but conjoining the *negation* of each per-theory
   atom, so a bare-variable disequality is "unequal in every applicable theory"
   rather than merely "not equal in every theory at once". *)
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
    let%bind.Or_error type_a = type_expr_of a in
    let%bind.Or_error type_b = type_expr_of b in
    let%bind.Or_error la_a = linear_expr_of a in
    let%bind.Or_error la_b = linear_expr_of b in
    Ok
      (F.And
         [ F.Not (F.Atom (`Eq (uf_a, uf_b)))
         ; F.Not (F.Atom (`Type_eq (type_a, type_b)))
         ; F.Not
             (F.And
                (List.map (le_atoms_of_eq la_a la_b) ~f:(fun atom ->
                   F.Atom atom)))
         ])
;;

let rec uf_term_to_formula : Uf_term.t -> [> `Uf ] Formula.t = function
  | `Var v -> Var v
  | `App (~function_, ~args) ->
    App (function_, List.map args ~f:uf_term_to_formula)
;;

let rec type_expr_to_formula : Type_expr.t -> [> `Type ] Formula.t = function
  | Var v -> Var v
  | Base Bool -> Bool
  | Base Int -> Int
  | Base Float -> Float
  | Type_of v -> Type_of (Var v)
  | App (f, [ arg ]) -> Type_app (f, type_expr_to_formula arg)
  | App (_, _) ->
    (* [Formula.Type_app] only represents single-argument application with a
       bare [Tvar.t] head, so multi-arg (or zero-arg) [Type_expr.App] can't be
       reconstructed as a [Formula.t]. *)
    raise_s
      [%message
        "Encoding.type_expr_to_formula: can't reconstruct a multi-argument \
         Type_expr.App as a Formula.t"]
  | Type_expr.Type -> Formula.Type
  | Function_type (a, b) ->
    Function_type (type_expr_to_formula a, type_expr_to_formula b)
;;

let linear_expr_to_formula (le : Linear_expr.t) : [> `La ] Formula.t =
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
  | `Eq (a, b) -> Eq (uf_term_to_formula a, uf_term_to_formula b)
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
