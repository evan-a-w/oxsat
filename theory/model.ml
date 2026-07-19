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
  ; (* Every EUF-registered term mapped to its equivalence-class representative,
       so equalities, disequalities, and congruence are all decidable from the
       model alone. *)
    euf_classes : Formula.any Formula.Any.Map.t
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

(* A type expression with no variables left to instantiate: fully determined. *)
let rec is_ground (type_expr : Type_expr.t) =
  match type_expr with
  | Var _ | Type_of _ -> false
  | Base _ | Type -> true
  | App (_, args) -> List.for_all args ~f:is_ground
  | Function_type (a, b) -> is_ground a && is_ground b
;;

(* Resolves a type expression to its assigned type when it is a variable with a
   known type; concrete constructors are their own witness. *)
let resolved_type t (type_expr : Type_expr.t) =
  match type_expr with
  | Var v -> Option.bind (Map.find t.tvar_assignments v) ~f:(fun a -> a.type_)
  | ty -> Some ty
;;

(* A type equality [a = b] is checkable when both sides resolve to *ground*
   witness types: [true] requires them equal, [false] requires them distinct. If
   a side is unconstrained (no witness / non-ground), the atom's truth is a free
   choice and not a model error. *)
let check_type_atom t ~a ~b ~expected =
  match resolved_type t a, resolved_type t b with
  | Some ta, Some tb when is_ground ta && is_ground tb ->
    let equal = [%compare.equal: Type_expr.t] ta tb in
    if Bool.equal equal expected
    then Ok ()
    else
      error
        [%message
          "type equality value disagrees with the assigned ground types"
            (ta : Type_expr.t)
            (tb : Type_expr.t)
            (expected : bool)
            ~types_equal:(equal : bool)]
  | _ -> Ok ()
;;

let repr t term = Map.find t.euf_classes term

(* An EUF equality atom is checkable iff both sides are registered terms with a
   known representative; then [true] requires equal reprs and [false] distinct
   reprs. *)
let check_euf_atom t ~a ~b ~expected =
  match repr t a, repr t b with
  | Some ra, Some rb ->
    let equal = Formula.compare_any ra rb = 0 in
    if Bool.equal equal expected
    then Ok ()
    else
      error
        [%message
          "EUF equality value disagrees with the equivalence classes"
            (a : Formula.any)
            (b : Formula.any)
            (expected : bool)
            ~classes_agree:(equal : bool)]
  | _ ->
    error
      [%message
        "EUF equality references a term with no class representative"
          (a : Formula.any)
          (b : Formula.any)]
;;

(* Per-atom theory consistency: each assigned atom's truth value must agree with
   the numeric/type/EUF witnesses. *)
let check_atom_consistency t ~atom ~value =
  match (atom : Atom.t) with
  | `Le (expression, bound) ->
    check_linear_atom t ~expression ~bound ~expected:value
  | `Type_eq (a, b) -> check_type_atom t ~a ~b ~expected:value
  | `Eq (a, b) -> check_euf_atom t ~a ~b ~expected:value
;;

(* Congruence: two registered terms with the same operator and pairwise
   class-equal arguments must be in the same class. Checked over all registered
   terms so the equivalence classes constitute a genuine congruence, not just an
   arbitrary partition consistent with the asserted (dis)equalities. *)
let check_congruence t =
  let terms = Map.keys t.euf_classes in
  let args_class_equal xs ys =
    match
      List.for_all2 xs ys ~f:(fun x y ->
        match repr t x, repr t y with
        | Some rx, Some ry -> Formula.compare_any rx ry = 0
        | _ -> false)
    with
    | Ok all_equal -> all_equal
    | Unequal_lengths -> false
  in
  List.fold_result terms ~init:() ~f:(fun () left ->
    List.fold_result terms ~init:() ~f:(fun () right ->
      if Formula.Op.compare (Formula.op left) (Formula.op right) = 0
         && args_class_equal (Formula.args left) (Formula.args right)
         && not
              (Formula.compare_any
                 (Map.find_exn t.euf_classes left)
                 (Map.find_exn t.euf_classes right)
               = 0)
      then
        error
          [%message
            "congruent terms are in different classes"
              (left : Formula.any)
              (right : Formula.any)]
      else Ok ()))
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
  let%bind.Or_error () =
    Map.fold t.atom_values ~init:(Ok ()) ~f:(fun ~key:atom ~data:value acc ->
      let%bind.Or_error () = acc in
      check_atom_consistency t ~atom ~value)
  in
  check_congruence t
;;
