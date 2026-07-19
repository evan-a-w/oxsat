open! Core
open! Import

(* A checkable model of a satisfiable query: the boolean value the solver
   assigned to each theory atom, plus what each theory determined about each
   [Tvar.t]. [check] verifies independently that the model satisfies the
   asserted formulas and that its atom values are consistent with the
   numeric/type/EUF witnesses. *)

type t =
  { atom_values : bool Atom.Map.t
  ; tvar_assignments : Tvar_assignment.t Tvar.Map.t
  }
[@@deriving sexp_of]

module Truth = struct
  type t =
    | True
    | False
    | Unknown

  let not_ = function
    | True -> False
    | False -> True
    | Unknown -> Unknown
  ;;
end

let atom_value t atom =
  match Map.find t.atom_values (Atom.normalize atom) with
  | Some value -> Truth.(if value then True else False)
  | None -> Truth.Unknown
;;

let rec eval t (formula : Theory_core.Boolean_formula.t) : Truth.t =
  match formula with
  | True -> True
  | False -> False
  | Atom atom -> atom_value t atom
  | Not formula -> Truth.not_ (eval t formula)
  | And formulas ->
    (* False dominates; else Unknown if any unknown; else True. *)
    List.fold formulas ~init:Truth.True ~f:(fun acc formula ->
      match acc, eval t formula with
      | False, _ | _, False -> False
      | Unknown, _ | _, Unknown -> Unknown
      | True, True -> True)
  | Or formulas ->
    List.fold formulas ~init:Truth.False ~f:(fun acc formula ->
      match acc, eval t formula with
      | True, _ | _, True -> True
      | Unknown, _ | _, Unknown -> Unknown
      | False, False -> False)
;;

let error = Or_error.error_s

(* Numeric value of a linear expression under the model, as [(value, eps_coeff)]
   in the same symbolic-infinitesimal representation the simplex uses. Returns
   [None] if any variable has no numeric assignment. *)
let eval_linear_expr t ({ coeffs; const } : Linear_expr.t) =
  Map.fold
    coeffs
    ~init:(Some (const, Q.zero))
    ~f:(fun ~key:tvar ~data:coeff acc ->
      let%bind.Option value, eps = acc in
      match Map.find t.tvar_assignments tvar with
      | Some { numeric = Some { value = v; eps_coeff = e }; _ } ->
        Some (Q.(value + (coeff * v)), Q.(eps + (coeff * e)))
      | _ -> None)
;;

(* A [`Le (e, c)] atom asserts [e <= c], i.e. [e - c <= 0] with the simplex
   epsilon-semantics: true iff [value < 0], or [value = 0] and [eps_coeff <= 0]. *)
let le_holds ~value ~eps_coeff =
  Q.sign' value < 0 || (Q.is_zero value && Q.sign' eps_coeff <= 0)
;;

let check_linear_atom t ~expression ~bound ~expected =
  match eval_linear_expr t expression with
  | None ->
    (* No numeric witness for some variable; the [`Le] atom's truth cannot be
       corroborated, but that is not a model error on its own. *)
    Ok ()
  | Some (value, eps_coeff) ->
    let value = Q.(value - bound) in
    let holds = le_holds ~value ~eps_coeff in
    if Bool.equal holds expected
    then Ok ()
    else
      error
        [%message
          "linear atom value disagrees with its model truth value"
            (expression : Linear_expr.t)
            (bound : Q.t)
            (expected : bool)
            (holds : bool)]
;;

let check_type_atom t ~a ~b ~expected =
  (* Only a true type equality between two ground-typed variables is checkable
     from [tvar_assignments]: their assigned types must then be equal. *)
  match a, b, expected with
  | Type_expr.Var va, Type_expr.Var vb, true ->
    let type_of v =
      Option.bind (Map.find t.tvar_assignments v) ~f:(fun a -> a.type_)
    in
    (match type_of va, type_of vb with
     | Some ta, Some tb when not ([%compare.equal: Type_expr.t] ta tb) ->
       error
         [%message
           "true type equality relates variables of different types"
             (va : Tvar.t)
             (ta : Type_expr.t)
             (vb : Tvar.t)
             (tb : Type_expr.t)]
     | _ -> Ok ())
  | _ -> Ok ()
;;

(* Per-atom theory consistency: each assigned atom's truth value must agree with
   the numeric/type witnesses in [tvar_assignments]. EUF equalities are checked
   at the boolean level (via [eval]); a deeper congruence check would require
   re-running the theory. *)
let check_atom_consistency t ~atom ~value =
  match (atom : Atom.t) with
  | `Le (expression, bound) ->
    check_linear_atom t ~expression ~bound ~expected:value
  | `Type_eq (a, b) -> check_type_atom t ~a ~b ~expected:value
  | `Eq _ -> Ok ()
;;

let check t ~asserted_formulas =
  let%bind.Or_error () =
    List.fold_result asserted_formulas ~init:() ~f:(fun () formula ->
      let%bind.Or_error boolean =
        Theory_core.Boolean_formula.of_formula formula
      in
      match eval t boolean with
      | True -> Ok ()
      | False ->
        error
          [%message
            "asserted formula is false under the model" (formula : Formula.any)]
      | Unknown ->
        error
          [%message
            "asserted formula is not determined true by the model's atom values"
              (formula : Formula.any)])
  in
  Map.fold t.atom_values ~init:(Ok ()) ~f:(fun ~key:atom ~data:value acc ->
    let%bind.Or_error () = acc in
    check_atom_consistency t ~atom ~value)
;;
