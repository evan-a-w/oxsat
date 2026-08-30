open! Core
open! Theory_core

let error message = Or_error.error_string message

let literal_at clause index =
  let literals = Proof_clause.literals clause in
  if index < 0 || index >= Array.length literals
  then
    Or_error.error_s
      [%message "clause literal index out of bounds" (index : int)]
  else Ok literals.(index)
;;

let theory_literal_at clause index =
  let%bind.Or_error literal = literal_at clause index in
  match literal.atom with
  | Theory atom -> Ok (atom, literal.positive)
  | Extension _ -> error "a theory certificate referenced an extension literal"
;;

let clause_equal clause literals =
  match Proof_clause.create literals with
  | `Tautology -> false
  | `Clause expected -> [%compare.equal: Proof_clause.t] clause expected
;;

let theory_literal atom positive =
  Proof_literal.create ~atom:(Theory atom) ~positive
;;

let eq_literal left right = theory_literal (`Eq (left, right)) true
let neq_literal left right = theory_literal (`Eq (left, right)) false

let check_array clause certificate =
  let open Proof_theory_certificate.Array in
  let eq = eq_literal in
  let neq = neq_literal in
  let not_has_type (var, type_expr) =
    theory_literal (`Has_type (var, type_expr)) false
  in
  let expected =
    match certificate with
    | Read_over_write_same_index { array; index; value } ->
      [ eq (Formula.Select (Formula.Store (array, index, value), index)) value ]
    | Read_over_write_different_index
        { array; written_index; written_value; read_index } ->
      [ eq written_index read_index
      ; eq
          (Formula.Select
             (Formula.Store (array, written_index, written_value), read_index))
          (Formula.Select (array, read_index))
      ]
    | Extensionality { left; right; witness; type_premises } ->
      List.map type_premises ~f:not_has_type
      @ [ eq left right
        ; neq (Formula.Select (left, witness)) (Formula.Select (right, witness))
        ]
  in
  if clause_equal clause expected
  then Ok ()
  else error "array certificate does not match its clause"
;;

let check_adt clause certificate ~datatype_env =
  let open Proof_theory_certificate.Adt in
  let constructor_term constructor args =
    Formula.Datatype_constructor (constructor, args)
  in
  let tester_atom constructor argument =
    `Eq (Formula.Datatype_tester (constructor, argument), Formula.True)
  in
  let selector_term selector argument =
    Formula.Datatype_selector (selector, argument)
  in
  let guard argument witness =
    if Formula.equal_any argument witness
    then []
    else [ neq_literal argument witness ]
  in
  let field args index =
    if index < 0 || index >= List.length args then None else List.nth args index
  in
  let constructor_is_declared constructor =
    Datatype.Env.mem_constructor datatype_env constructor
  in
  let selector_is_declared selector =
    Datatype.Env.mem_selector datatype_env selector
  in
  let reconstruction_args constructor argument =
    let%bind.Option declaration =
      Datatype.Env.find_constructor datatype_env constructor
    in
    List.init constructor.arity ~f:(fun index ->
      List.find declaration.selectors ~f:(fun selector ->
        selector.index = index)
      |> Option.map ~f:(fun selector -> selector_term selector argument))
    |> Option.all
  in
  let expected =
    match certificate with
    | Injectivity { constructor; left_args; right_args; field_index } ->
      if not (constructor_is_declared constructor)
      then None
      else
        Option.map2
          (field left_args field_index)
          (field right_args field_index)
          ~f:(fun left right ->
            [ neq_literal
                (constructor_term constructor left_args)
                (constructor_term constructor right_args)
            ; eq_literal left right
            ])
    | Disjointness
        { left_constructor; left_args; right_constructor; right_args } ->
      if constructor_is_declared left_constructor
         && constructor_is_declared right_constructor
         && Datatype.Datatype.equal
              left_constructor.datatype
              right_constructor.datatype
         && not (Datatype.Constructor.equal left_constructor right_constructor)
      then
        Some
          [ neq_literal
              (constructor_term left_constructor left_args)
              (constructor_term right_constructor right_args)
          ]
      else None
    | Tester
        { tester_constructor
        ; argument
        ; witness_constructor
        ; witness_args
        ; value
        } ->
      let witness = constructor_term witness_constructor witness_args in
      if constructor_is_declared tester_constructor
         && constructor_is_declared witness_constructor
         && Datatype.Datatype.equal
              tester_constructor.datatype
              witness_constructor.datatype
         && Bool.equal
              value
              (Datatype.Constructor.equal
                 tester_constructor
                 witness_constructor)
      then
        Some
          (guard argument witness
           @ [ theory_literal (tester_atom tester_constructor argument) value ]
          )
      else None
    | Tester_exclusivity
        { left_constructor; left_argument; right_constructor; right_argument }
      ->
      if constructor_is_declared left_constructor
         && constructor_is_declared right_constructor
         && Datatype.Datatype.equal
              left_constructor.datatype
              right_constructor.datatype
         && not (Datatype.Constructor.equal left_constructor right_constructor)
      then
        Some
          (guard left_argument right_argument
           @ [ theory_literal (tester_atom left_constructor left_argument) false
             ; theory_literal
                 (tester_atom right_constructor right_argument)
                 false
             ])
      else None
    | Tester_reconstruction { constructor; argument } ->
      if constructor_is_declared constructor
      then
        Option.map (reconstruction_args constructor argument) ~f:(fun args ->
          [ theory_literal (tester_atom constructor argument) false
          ; eq_literal argument (constructor_term constructor args)
          ])
      else None
    | Selector { selector; argument; constructor_args } ->
      let constructor = selector.constructor in
      let witness = constructor_term constructor constructor_args in
      if not (selector_is_declared selector)
      then None
      else
        Option.map (field constructor_args selector.index) ~f:(fun projected ->
          guard argument witness
          @ [ eq_literal (selector_term selector argument) projected ])
    | Acyclicity { cycle } ->
      (match cycle with
       | [] -> None
       | _ ->
         let fields = List.map cycle ~f:(fun edge -> edge.Cycle_edge.field) in
         let previous_fields =
           match List.rev fields with
           | [] -> []
           | last :: rev_rest -> last :: List.rev rev_rest
         in
         let valid_edges =
           List.for_all cycle ~f:(fun { Cycle_edge.constructor_term; field } ->
             match constructor_term with
             | Formula.Datatype_constructor (_, args) ->
               List.exists args ~f:(Formula.equal_any field)
             | _ -> false)
         in
         if valid_edges
         then
           Some
             (List.map2_exn cycle previous_fields ~f:(fun edge previous_field ->
                neq_literal edge.constructor_term previous_field))
         else None)
    | Completeness { declaration; subject; guard; form } ->
      let declaration_is_active =
        match Datatype.Env.find datatype_env declaration.datatype with
        | Some active -> Datatype.Declaration.equal active declaration
        | None -> false
      in
      let guard_literals =
        match guard with
        | None -> []
        | Some atom -> [ theory_literal (atom :> Atom.t) false ]
      in
      let constructor_declarations = declaration.constructors in
      let constructors =
        List.map constructor_declarations ~f:(fun cd -> cd.constructor)
      in
      if not declaration_is_active
      then None
      else (
        match (form : Completeness_form.t) with
        | Enum_equalities ->
          if List.for_all constructors ~f:(fun constructor ->
               constructor.arity = 0)
          then
            Some
              (guard_literals
               @ List.map constructors ~f:(fun constructor ->
                 eq_literal subject (constructor_term constructor [])))
          else None
        | Testers ->
          Some
            (guard_literals
             @ List.map constructors ~f:(fun constructor ->
               theory_literal (tester_atom constructor subject) true)))
  in
  match expected with
  | Some expected when clause_equal clause expected -> Ok ()
  | Some _ | None -> error "ADT certificate does not match its clause"
;;

let check_bare_var_eq clause certificate =
  let open Proof_theory_certificate.Bare_var_eq in
  let uf a b : Atom.t = `Eq (Formula.Var a, Formula.Var b) in
  let type_eq a b : Atom.t = `Type_eq (Type_expr.Var a, Type_expr.Var b) in
  let le_atoms a b =
    let diff = Linear_expr.(var a - var b) in
    `Le (diff, Q.zero), `Le (Linear_expr.neg diff, Q.zero)
  in
  let expected =
    match certificate with
    | Equality_implies_type_equality (a, b) ->
      [ theory_literal (type_eq a b) true; theory_literal (uf a b) false ]
    | Equality_implies_has_type { source; target; type_ } ->
      [ theory_literal (`Has_type (target, type_)) true
      ; theory_literal (`Has_type (source, type_)) false
      ; theory_literal (uf source target) false
      ]
    | Equality_implies_le { left; right; direction } ->
      let left_le_right, right_le_left = le_atoms left right in
      let le =
        match direction with
        | Left_le_right -> left_le_right
        | Right_le_left -> right_le_left
      in
      [ theory_literal le true; theory_literal (uf left right) false ]
    | Numeric_coincidence_implies_equality (a, b) ->
      let le1, le2 = le_atoms a b in
      [ theory_literal (uf a b) true
      ; theory_literal le1 false
      ; theory_literal le2 false
      ]
  in
  if clause_equal clause expected
  then Ok ()
  else error "bare-variable equality certificate does not match its clause"
;;

let check_integer_split
  clause
  ({ guard; variable; floor; ceil } : Proof_theory_certificate.Integer_split.t)
  =
  if not (Q.equal ceil (Q.( + ) floor Q.one))
  then error "integer split bounds are not adjacent"
  else (
    match guard with
    | `Has_type (guard_variable, type_expr)
      when Tvar.equal guard_variable variable
           && Option.value_map
                (Numeric_domain.of_type_expr type_expr)
                ~default:false
                ~f:(fun domain -> domain.integral) ->
      let expected =
        [ theory_literal guard false
        ; theory_literal (`Le (Linear_expr.var variable, floor)) true
        ; theory_literal
            (`Le (Linear_expr.neg (Linear_expr.var variable), Q.neg ceil))
            true
        ]
      in
      if clause_equal clause expected
      then Ok ()
      else error "integer split certificate does not match its clause"
    | _ -> error "integer split guard is not an integral type premise")
;;

let check_type_domain
  clause
  ({ guard; consequence } : Proof_theory_certificate.Type_domain.t)
  =
  let valid =
    match guard, consequence with
    | `Has_type (variable, type_expr), `Le (expr, bound) ->
      (match Numeric_domain.of_type_expr type_expr with
       | Some { bounds = Some { lower; upper }; _ } ->
         ([%compare.equal: Linear_expr.t] expr (Linear_expr.var variable)
          && Q.equal bound upper)
         || ([%compare.equal: Linear_expr.t]
               expr
               (Linear_expr.neg (Linear_expr.var variable))
             && Q.equal bound (Q.neg lower))
       | Some { bounds = None; _ } | None -> false)
    | ( `Has_type (guard_variable, subtype)
      , `Has_type (consequence_variable, supertype) ) ->
      Tvar.equal guard_variable consequence_variable
      && Type_lattice.is_subtype subtype ~of_:supertype
    | _ -> false
  in
  if not valid
  then error "type-domain certificate is not a valid type-domain implication"
  else if clause_equal
            clause
            [ theory_literal guard false; theory_literal consequence true ]
  then Ok ()
  else error "type-domain certificate does not match its clause"
;;

let check_linear_arithmetic
  clause
  ({ combination } : Proof_theory_certificate.Linear_arithmetic.t)
  =
  let strict = ref false in
  let%bind.Or_error sum =
    List.fold_result combination ~init:Linear_expr.zero ~f:(fun sum term ->
      if Q.sign' term.coefficient < 0
      then error "Farkas coefficients must be non-negative"
      else (
        let%bind.Or_error atom, positive =
          theory_literal_at clause term.clause_literal
        in
        match atom with
        | `Le (expression, bound) ->
          let assumed_value = not positive in
          let expression = Linear_expr.(expression - const bound) in
          let expression =
            if assumed_value
            then expression
            else (
              if not (Q.is_zero term.coefficient) then strict := true;
              Linear_expr.neg expression)
          in
          Ok Linear_expr.(sum + scale term.coefficient expression)
        | `Eq _ | `Type_eq _ | `Has_type _ ->
          error "linear certificate referenced a non-linear atom"))
  in
  if not (Map.is_empty sum.coeffs)
  then error "Farkas combination does not eliminate every variable"
  else if Q.sign' sum.const > 0 || (Q.is_zero sum.const && !strict)
  then Ok ()
  else error "Farkas combination does not produce a contradiction"
;;

let structurally_incompatible left right = Type_lattice.disjoint left right

let type_assignment atom positive =
  match atom, positive with
  | `Has_type (variable, type_), false -> Some (variable, type_)
  | `Eq _, _ | `Le _, _ | `Type_eq _, _ | `Has_type _, _ -> None
;;

let check_type_theory
  clause
  ({ left; right; premise_literals } : Proof_theory_certificate.Type_theory.t)
  =
  if not (structurally_incompatible left right)
  then error "type certificate types are not structurally incompatible"
  else (
    let%bind.Or_error assignments =
      Or_error.all
        (List.map premise_literals ~f:(fun index ->
           let%bind.Or_error atom, positive = theory_literal_at clause index in
           match type_assignment atom positive with
           | Some assignment -> Ok assignment
           | None ->
             error "type premise is not a negated variable type membership"))
    in
    match assignments with
    | [ (variable1, type1); (variable2, type2) ]
      when Tvar.equal variable1 variable2
           && ((Type_expr.compare type1 left = 0
                && Type_expr.compare type2 right = 0)
               || (Type_expr.compare type1 right = 0
                   && Type_expr.compare type2 left = 0)) -> Ok ()
    | _ -> error "type certificate premises do not establish the claimed clash")
;;

let formula_equal a b = Formula.compare_any a b = 0

let equality_equal
  ({ left = a1; right = b1 } : Proof_theory_certificate.Euf.Equality.t)
  ({ left = a2; right = b2 } : Proof_theory_certificate.Euf.Equality.t)
  =
  (formula_equal a1 a2 && formula_equal b1 b2)
  || (formula_equal a1 b2 && formula_equal b1 a2)
;;

let connected edges left right =
  let rec visit seen = function
    | [] -> false
    | node :: rest ->
      if formula_equal node right
      then true
      else if Set.mem seen node
      then visit seen rest
      else (
        let seen = Set.add seen node in
        let neighbors =
          List.filter_map edges ~f:(fun (a, b) ->
            if formula_equal node a
            then Some b
            else if formula_equal node b
            then Some a
            else None)
        in
        visit seen (neighbors @ rest))
  in
  formula_equal left right || visit Formula.Any.Set.empty [ left ]
;;

(* An equality atom as a pair of [Formula.any] endpoints. A [`Type_eq] is the
   equality of its type expressions widened to formulas, so the EUF checker can
   reason over the type-level congruence closure with [`Type_eq] clause literals
   (which the encoding uses for type-role equalities). *)
let equality_endpoints : Proof_atom.t -> (Formula.any * Formula.any) option
  = function
  | Theory (`Eq (left, right)) -> Some (left, right)
  | Theory (`Type_eq (left, right)) ->
    Some (Formula.type_expr_to_formula left, Formula.type_expr_to_formula right)
  | Theory (`Le _ | `Has_type _) | Extension _ -> None
;;

let check_equality_proof
  clause
  ({ conclusion; path } : Proof_theory_certificate.Euf.Equality_proof.t)
  =
  let%bind.Or_error edges =
    List.fold_result path ~init:[] ~f:(fun edges justification ->
      match justification with
      | Asserted { clause_literal } ->
        let%bind.Or_error literal = literal_at clause clause_literal in
        (match equality_endpoints literal.atom, literal.positive with
         | Some (left, right), false -> Ok ((left, right) :: edges)
         | Some _, true | None, _ ->
           error "EUF asserted edge is not an assumed equality")
      | Congruence { left; right; argument_equalities } ->
        let left_args = Formula.args left in
        let right_args = Formula.args right in
        let expected_equalities =
          match List.zip left_args right_args with
          | Unequal_lengths -> None
          | Ok pairs ->
            Some
              (List.map pairs ~f:(fun (left, right) ->
                 { Proof_theory_certificate.Euf.Equality.left; right }))
        in
        (match expected_equalities with
         | None -> error "congruence terms have different arities"
         | Some expected
           when Formula.Op.compare (Formula.op left) (Formula.op right) <> 0
                || List.compare
                     Proof_theory_certificate.Euf.Equality.compare
                     expected
                     argument_equalities
                   <> 0 ->
           error "congruence justification does not match its terms"
         | Some _ ->
           if List.for_all argument_equalities ~f:(fun equality ->
                connected edges equality.left equality.right)
           then Ok ((left, right) :: edges)
           else error "congruence argument equality has not been justified"))
  in
  if connected edges conclusion.left conclusion.right
  then Ok ()
  else error "EUF path does not establish its conclusion"
;;

let clause_has_equality clause equality ~positive =
  Proof_clause.literals clause
  |> Array.exists ~f:(fun literal ->
    Bool.equal literal.positive positive
    &&
    match equality_endpoints literal.atom with
    | Some (left, right) ->
      equality_equal
        equality
        { Proof_theory_certificate.Euf.Equality.left; right }
    | None -> false)
;;

let check_euf clause certificate =
  match certificate with
  | Proof_theory_certificate.Euf.Equality proof ->
    if not (clause_has_equality clause proof.conclusion ~positive:true)
    then error "EUF equality conclusion is not a positive clause literal"
    else check_equality_proof clause proof
  | Disequality { conclusion; asserted_disequality; left_path; right_path } ->
    if not (clause_has_equality clause conclusion ~positive:false)
    then error "EUF disequality conclusion is not a negative clause literal"
    else (
      let%bind.Or_error literal = literal_at clause asserted_disequality in
      match equality_endpoints literal.atom, literal.positive with
      | Some (asserted_left, asserted_right), true ->
        let direct =
          equality_equal
            left_path.conclusion
            { left = conclusion.left; right = asserted_left }
          && equality_equal
               right_path.conclusion
               { left = conclusion.right; right = asserted_right }
        in
        let swapped =
          equality_equal
            left_path.conclusion
            { left = conclusion.left; right = asserted_right }
          && equality_equal
               right_path.conclusion
               { left = conclusion.right; right = asserted_left }
        in
        if not (direct || swapped)
        then
          error "EUF disequality transport paths do not connect the endpoints"
        else (
          let%bind.Or_error () = check_equality_proof clause left_path in
          check_equality_proof clause right_path)
      | Some _, false | None, _ ->
        error "EUF disequality premise is not an assumed disequality")
;;

let check ?(datatype_env = Datatype.Env.empty) ~clause = function
  | Proof_theory_certificate.Bare_var_eq certificate ->
    check_bare_var_eq clause certificate
  | Integer_split certificate -> check_integer_split clause certificate
  | Type_domain certificate -> check_type_domain clause certificate
  | Linear_arithmetic certificate -> check_linear_arithmetic clause certificate
  | Type_theory certificate -> check_type_theory clause certificate
  | Array certificate -> check_array clause certificate
  | Adt certificate -> check_adt clause certificate ~datatype_env
  | Euf certificate -> check_euf clause certificate
;;
