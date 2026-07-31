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
  | Function_type (a, b) | Array_type (a, b) -> is_ground a && is_ground b
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
let reps_equal t a b =
  match repr t a, repr t b with
  | Some ra, Some rb -> Formula.compare_any ra rb = 0
  | _ -> false
;;

let check_arrays t =
  let terms = Map.keys t.euf_classes in
  let known_arrays = Formula.Any.Hash_set.create () in
  let rec note = function
    | Formula.Select (array, index) ->
      Hash_set.add known_arrays array;
      note array;
      note index
    | Store (array, index, value) as store ->
      Hash_set.add known_arrays store;
      Hash_set.add known_arrays array;
      note array;
      note index;
      note value
    | term -> List.iter (Formula.args term) ~f:note
  in
  List.iter terms ~f:note;
  let is_array = function
    | Formula.Store _ -> true
    | term -> Hash_set.mem known_arrays term
  in
  let%bind.Or_error () =
    List.fold_result terms ~init:() ~f:(fun () -> function
      | Formula.Select
          ((Formula.Store (array, index, value) as store), read_index) as select
        ->
        let same_index_select = Formula.Select (store, index) in
        let%bind.Or_error () =
          if Formula.equal_any index read_index || reps_equal t index read_index
          then
            if reps_equal t select value
            then Ok ()
            else
              error
                [%message
                  "array read-over-write/same-index axiom is violated"
                    (array : Formula.any)
                    (index : Formula.any)
                    (value : Formula.any)]
          else Ok ()
        in
        if Map.mem t.euf_classes same_index_select
           && not (reps_equal t same_index_select value)
        then
          error
            [%message
              "array read-over-write/same-index axiom is violated"
                (array : Formula.any)
                (index : Formula.any)
                (value : Formula.any)]
        else if Formula.equal_any index read_index
                || reps_equal t index read_index
        then Ok ()
        else if reps_equal t select (Formula.Select (array, read_index))
        then Ok ()
        else
          error
            [%message
              "array read-over-write/different-index axiom is violated"
                (array : Formula.any)
                (index : Formula.any)
                (read_index : Formula.any)]
      | _ -> Ok ())
  in
  Map.fold t.atom_values ~init:(Ok ()) ~f:(fun ~key:atom ~data:value acc ->
    let%bind.Or_error () = acc in
    match atom, value with
    | `Eq (left, right), false when is_array left && is_array right ->
      let has_witness =
        List.exists terms ~f:(function
          | Formula.Select (select_left, witness)
            when Formula.equal_any select_left left ->
            let left_select = Formula.Select (left, witness) in
            let right_select = Formula.Select (right, witness) in
            Map.mem t.euf_classes right_select
            && not (reps_equal t left_select right_select)
          | _ -> false)
      in
      if has_witness
      then Ok ()
      else
        error
          [%message
            "array extensionality axiom is not witnessed in the model"
              (left : Formula.any)
              (right : Formula.any)]
    | _ -> Ok ())
;;

let check_adts t ~datatype_env ~adt_observations =
  let terms = Map.keys t.euf_classes in
  let constructors =
    List.filter_map terms ~f:(function
      | Formula.Datatype_constructor (constructor, args) as term ->
        Some (constructor, args, term)
      | _ -> None)
  in
  let selectors =
    List.filter_map terms ~f:(function
      | Formula.Datatype_selector (selector, argument) as term ->
        Some (selector, argument, term)
      | _ -> None)
  in
  let testers =
    List.filter_map terms ~f:(function
      | Formula.Datatype_tester (constructor, argument) as term ->
        Some (constructor, argument, term)
      | _ -> None)
  in
  let%bind.Or_error () =
    List.fold_result
      constructors
      ~init:()
      ~f:(fun () (left_c, left_args, left) ->
        List.fold_result
          constructors
          ~init:()
          ~f:(fun () (right_c, right_args, right) ->
            if reps_equal t left right
            then
              if Datatype.Datatype.equal left_c.datatype right_c.datatype
                 && not (Datatype.Constructor.equal left_c right_c)
              then
                error
                  [%message
                    "ADT constructor disjointness is violated"
                      (left : Formula.any)
                      (right : Formula.any)]
              else if Datatype.Constructor.equal left_c right_c
              then (
                match
                  List.for_all2
                    left_args
                    right_args
                    ~f:(fun left_arg right_arg ->
                      reps_equal t left_arg right_arg)
                with
                | Ok true | Unequal_lengths -> Ok ()
                | Ok false ->
                  error
                    [%message
                      "ADT constructor injectivity is violated"
                        (left : Formula.any)
                        (right : Formula.any)])
              else Ok ()
            else Ok ()))
  in
  let%bind.Or_error () =
    List.fold_result selectors ~init:() ~f:(fun () (selector, argument, term) ->
      List.fold_result
        constructors
        ~init:()
        ~f:(fun () (constructor, args, witness) ->
          if Datatype.Constructor.equal selector.constructor constructor
             && reps_equal t argument witness
          then (
            match List.nth args selector.index with
            | Some projected when reps_equal t term projected -> Ok ()
            | Some projected ->
              error
                [%message
                  "ADT selector projection is violated"
                    ~selector_term:(term : Formula.any)
                    (projected : Formula.any)]
            | None -> Ok ())
          else Ok ()))
  in
  let%bind.Or_error () =
    List.fold_result
      testers
      ~init:()
      ~f:(fun () (tested_constructor, argument, tester) ->
        List.fold_result
          constructors
          ~init:()
          ~f:(fun () (constructor, _, witness) ->
            if Datatype.Datatype.equal
                 tested_constructor.datatype
                 constructor.datatype
               && reps_equal t argument witness
            then (
              let expected =
                Datatype.Constructor.equal tested_constructor constructor
              in
              match atom_value t (`Eq (tester, Formula.True)) with
              | True when expected -> Ok ()
              | False when not expected -> Ok ()
              | Unknown -> Ok ()
              | True | False ->
                error
                  [%message
                    "ADT tester value is violated"
                      (tester : Formula.any)
                      (expected : bool)])
            else Ok ()))
  in
  let edges =
    List.concat_map constructors ~f:(fun (_, args, term) ->
      let source = Map.find_exn t.euf_classes term in
      List.map args ~f:(fun field -> source, Map.find_exn t.euf_classes field))
  in
  let adjacency = Formula.Any.Table.create () in
  List.iter edges ~f:(fun (source, target) ->
    Hashtbl.add_multi adjacency ~key:source ~data:target);
  let rec reaches ~start seen node =
    if Formula.equal_any node start
    then true
    else if Set.mem seen node
    then false
    else (
      let seen = Set.add seen node in
      Hashtbl.find_multi adjacency node |> List.exists ~f:(reaches ~start seen))
  in
  let%bind.Or_error () =
    match
      List.find edges ~f:(fun (source, target) ->
        reaches ~start:source Formula.Any.Set.empty target)
    with
    | None -> Ok ()
    | Some _ -> error [%message "ADT acyclicity is violated"]
  in
  Map.fold
    adt_observations
    ~init:(Ok ())
    ~f:(fun ~key:subject ~data:datatypes acc ->
      let%bind.Or_error () = acc in
      Set.fold datatypes ~init:(Ok ()) ~f:(fun acc datatype ->
        let%bind.Or_error () = acc in
        match Datatype.Env.find datatype_env datatype with
        | None -> Ok ()
        | Some declaration ->
          let constructor_declarations = declaration.constructors in
          if List.for_all constructor_declarations ~f:(fun cd ->
               cd.constructor.arity = 0)
          then (
            let is_equal_to_some_constructor =
              List.exists constructor_declarations ~f:(fun cd ->
                reps_equal
                  t
                  subject
                  (Formula.Datatype_constructor (cd.constructor, [])))
            in
            if is_equal_to_some_constructor
            then Ok ()
            else
              error
                [%message
                  "ADT enum exhaustiveness is violated"
                    (subject : Formula.any)
                    (datatype : Datatype.Datatype.t)])
          else (
            let all_testers_false =
              List.for_all constructor_declarations ~f:(fun cd ->
                match
                  atom_value
                    t
                    (`Eq
                      ( Formula.Datatype_tester (cd.constructor, subject)
                      , Formula.True ))
                with
                | False -> true
                | True | Unknown -> false)
            in
            if all_testers_false
            then
              error
                [%message
                  "ADT constructor completeness is violated"
                    (subject : Formula.any)
                    (datatype : Datatype.Datatype.t)]
            else Ok ())))
;;

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

let check
  ?(datatype_env = Datatype.Env.empty)
  ?(adt_observations = Formula.Any.Map.empty)
  t
  ~asserted_formulas
  =
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
  let%bind.Or_error () = check_adts t ~datatype_env ~adt_observations in
  let%bind.Or_error () = check_congruence t in
  check_arrays t
;;
