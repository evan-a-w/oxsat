open! Core
open! Feel.Import

type t =
  | True
  | False
  | Atom of Atom.t
  | Not of t
  | And of t list
  | Or of t list
[@@deriving sexp, compare]

module Shape = struct
  type t =
    | Bool
    | Uf
    | Type
    | La
    | Var
end

let shape_of (type a) (formula : a Formula.t) : Shape.t =
  match formula with
  | Var _ -> Var
  | Eq _ | True | False | Not _ | And _ | Or _ -> Bool
  | App _ -> Uf
  | Bool
  | Int
  | Float
  | Type
  | Function_type _
  | Type_of _
  | Type_var _
  | Type_app _ -> Type
  | La_const _ | La_scale_const _ | La_add _ | La_compare _ -> La
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
  | Var v | Type_var v -> Ok (Type_expr.Var v)
  | Bool -> Ok (Type_expr.Base Bool)
  | Int -> Ok (Type_expr.Base Int)
  | Float -> Ok (Type_expr.Base Float)
  | Type -> Ok Type_expr.Type
  | Function_type (a, b) ->
    let%bind.Or_error a = type_expr_of a in
    let%bind.Or_error b = type_expr_of b in
    Ok (Type_expr.Function_type (a, b))
  | Type_of (Var v) -> Ok (Type_expr.Type_of v)
  | Type_of _ ->
    Or_error.error_s
      [%message "Type_of is only supported applied to a variable"]
  | Type_app (f, args) ->
    let%bind.Or_error args = Or_error.all (List.map args ~f:type_expr_of) in
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

let le_atoms_of_eq a b =
  let diff = Linear_expr.(a - b) in
  [ `Le (diff, Q.zero); `Le (Linear_expr.neg diff, Q.zero) ]
;;

let compare_atom a op b =
  match op with
  | `Le -> Atom (`Le (Linear_expr.(a - b), Q.zero))
  | `Ge -> Atom (`Le (Linear_expr.(b - a), Q.zero))
  | `Lt -> Not (Atom (`Le (Linear_expr.(b - a), Q.zero)))
  | `Gt -> Not (Atom (`Le (Linear_expr.(a - b), Q.zero)))
;;

let rec of_formula : Formula.any -> t Or_error.t = function
  | True -> Ok True
  | False -> Ok False
  | Not (Eq (a, b)) -> neq_formula_of a b
  | Not formula ->
    let%map.Or_error formula = of_formula formula in
    Not formula
  | And formulas ->
    let%map.Or_error formulas =
      Or_error.all (List.map formulas ~f:of_formula)
    in
    And formulas
  | Or formulas ->
    let%map.Or_error formulas =
      Or_error.all (List.map formulas ~f:of_formula)
    in
    Or formulas
  | La_compare (a, op, b) ->
    let%bind.Or_error a = linear_expr_of a in
    let%map.Or_error b = linear_expr_of b in
    compare_atom a op b
  | Eq (a, b) -> eq_formula_of a b
  | Var _ -> Or_error.error_s [%message "a bare variable is not a formula"]
  | _ -> Or_error.error_s [%message "formula is not boolean"]

and eq_formula_of : type a. a Formula.t -> a Formula.t -> t Or_error.t =
  fun a b ->
  match shape_of a, shape_of b with
  | Bool, _ | _, Bool ->
    let%bind.Or_error a = of_formula (Formula.widen a) in
    let%map.Or_error b = of_formula (Formula.widen b) in
    And [ Or [ Not a; b ]; Or [ a; Not b ] ]
  | Type, _ | _, Type ->
    let%bind.Or_error a = type_expr_of a in
    let%map.Or_error b = type_expr_of b in
    Atom (`Type_eq (a, b))
  | La, _ | _, La ->
    let%bind.Or_error a = linear_expr_of a in
    let%map.Or_error b = linear_expr_of b in
    And (List.map (le_atoms_of_eq a b) ~f:(fun atom -> Atom atom))
  | Uf, _ | _, Uf | Var, Var ->
    let%bind.Or_error a = uf_term_of a in
    let%map.Or_error b = uf_term_of b in
    Atom (`Eq (a, b))

and neq_formula_of : type a. a Formula.t -> a Formula.t -> t Or_error.t =
  fun a b ->
  match shape_of a, shape_of b with
  | Bool, _ | _, Bool ->
    let%map.Or_error equality = eq_formula_of a b in
    Not equality
  | Type, _ | _, Type ->
    let%bind.Or_error a = type_expr_of a in
    let%map.Or_error b = type_expr_of b in
    Not (Atom (`Type_eq (a, b)))
  | La, _ | _, La ->
    let%bind.Or_error a = linear_expr_of a in
    let%map.Or_error b = linear_expr_of b in
    Not (And (List.map (le_atoms_of_eq a b) ~f:(fun atom -> Atom atom)))
  | Uf, _ | _, Uf | Var, Var ->
    let%bind.Or_error a = uf_term_of a in
    let%map.Or_error b = uf_term_of b in
    Not (Atom (`Eq (a, b)))
;;
