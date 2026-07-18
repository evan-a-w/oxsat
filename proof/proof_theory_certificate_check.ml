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
  | `Clause expected -> Proof_clause.compare clause expected = 0
;;

let theory_literal atom positive =
  Proof_literal.create ~atom:(Theory atom) ~positive
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
  ({ variable; floor; ceil } : Proof_theory_certificate.Integer_split.t)
  =
  if not (Q.equal ceil (Q.( + ) floor Q.one))
  then error "integer split bounds are not adjacent"
  else (
    let expected =
      [ theory_literal
          (`Type_eq (Type_expr.Var variable, Type_expr.Base Int))
          false
      ; theory_literal (`Le (Linear_expr.var variable, floor)) true
      ; theory_literal
          (`Le (Linear_expr.neg (Linear_expr.var variable), Q.neg ceil))
          true
      ]
    in
    if clause_equal clause expected
    then Ok ()
    else error "integer split certificate does not match its clause")
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
        | `Eq _ | `Type_eq _ ->
          error "linear certificate referenced a non-linear atom"))
  in
  if not (Map.is_empty sum.coeffs)
  then error "Farkas combination does not eliminate every variable"
  else if Q.sign' sum.const > 0 || (Q.is_zero sum.const && !strict)
  then Ok ()
  else error "Farkas combination does not produce a contradiction"
;;

let structurally_incompatible left right =
  match left, right with
  | Type_expr.Base a, Type_expr.Base b -> not (Type_expr.Base.equal a b)
  | Type_expr.App (a, _), Type_expr.App (b, _) -> not (Tvar.equal a b)
  | Type_expr.Function_type _, Type_expr.Function_type _
  | Type_expr.Type, Type_expr.Type -> false
  | Type_expr.Var _, _
  | _, Type_expr.Var _
  | Type_expr.Type_of _, _
  | _, Type_expr.Type_of _ -> false
  | ( Type_expr.Base _
    , (Type_expr.App _ | Type_expr.Function_type _ | Type_expr.Type) )
  | ( Type_expr.App _
    , (Type_expr.Base _ | Type_expr.Function_type _ | Type_expr.Type) )
  | ( Type_expr.Function_type _
    , (Type_expr.Base _ | Type_expr.App _ | Type_expr.Type) )
  | ( Type_expr.Type
    , (Type_expr.Base _ | Type_expr.App _ | Type_expr.Function_type _) ) -> true
;;

let type_assignment atom positive =
  match atom, positive with
  | `Type_eq (Type_expr.Var variable, type_), false
  | `Type_eq (type_, Type_expr.Var variable), false -> Some (variable, type_)
  | `Eq _, _ | `Le _, _ | `Type_eq _, _ -> None
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
             error "type premise is not a negated variable type equality"))
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

let check_equality_proof
  clause
  ({ conclusion; path } : Proof_theory_certificate.Euf.Equality_proof.t)
  =
  let%bind.Or_error edges =
    List.fold_result path ~init:[] ~f:(fun edges justification ->
      match justification with
      | Asserted { clause_literal } ->
        let%bind.Or_error atom, positive =
          theory_literal_at clause clause_literal
        in
        (match atom, positive with
         | `Eq (left, right), false -> Ok ((left, right) :: edges)
         | `Eq _, true | `Le _, _ | `Type_eq _, _ ->
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
    match literal.atom with
    | Theory (`Eq (left, right)) ->
      equality_equal
        equality
        { Proof_theory_certificate.Euf.Equality.left; right }
    | Theory (`Le _ | `Type_eq _) | Extension _ -> false)
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
      let%bind.Or_error atom, positive =
        theory_literal_at clause asserted_disequality
      in
      match atom, positive with
      | `Eq (asserted_left, asserted_right), true ->
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
      | `Eq _, false | `Le _, _ | `Type_eq _, _ ->
        error "EUF disequality premise is not an assumed disequality")
;;

let check ~clause = function
  | Proof_theory_certificate.Bare_var_eq certificate ->
    check_bare_var_eq clause certificate
  | Integer_split certificate -> check_integer_split clause certificate
  | Linear_arithmetic certificate -> check_linear_arithmetic clause certificate
  | Type_theory certificate -> check_type_theory clause certificate
  | Euf certificate -> check_euf clause certificate
;;
