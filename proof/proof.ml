open! Core
open! Theory_core
module Id = Proof_id
module Atom = Proof_atom
module Boolean = Proof_boolean
module Literal = Proof_literal
module Clause = Proof_clause
module Extension = Proof_extension
module Theory_certificate = Proof_theory_certificate
module Refutation = Refutation

module Assumption = struct
  type t =
    { name : string option
    ; formula : Formula.any
    }
  [@@deriving sexp, compare]
end

module Rewrite_direction = struct
  type t =
    | Left_to_right
    | Right_to_left
  [@@deriving sexp, compare]
end

module Kernel_rule = struct
  type t =
    | Propositional
    | Equality_refl
    | Equality_symm
    | Equality_trans
    | Congruence
    | Rewrite of
        { direction : Rewrite_direction.t
        ; path : int list
        }
  [@@deriving sexp, compare]
end

module Justification = struct
  type t =
    | Assumption of Proof_id.Assumption.t
    | Kernel of
        { rule : Kernel_rule.t
        ; premises : Proof_id.Step.t array
        }
    | By_refutation of
        { premises : Proof_id.Step.t array
        ; refutation : Refutation.t
        }
  [@@deriving sexp, compare]
end

module Step = struct
  type t =
    { name : string option
    ; conclusion : Formula.any
    ; justification : Justification.t
    }
  [@@deriving sexp, compare]
end

type t =
  { assumptions : Assumption.t array
  ; steps : Step.t array
  ; conclusion : Proof_id.Step.t
  }
[@@deriving sexp, compare]

let error message = Or_error.error_string message
let formula_equal left right = Formula.compare_any left right = 0

let equality = function
  | Formula.Eq (left, right) -> Some (Formula.widen left, Formula.widen right)
  | _ -> None
;;

let equality_connects (a, b) left right =
  (formula_equal a left && formula_equal b right)
  || (formula_equal a right && formula_equal b left)
;;

let check_propositional premises conclusion =
  let%bind.Or_error premises =
    Or_error.all (List.map premises ~f:Boolean.of_formula)
  in
  let%bind.Or_error conclusion = Boolean.of_formula conclusion in
  if Proof_boolean_check.entails ~assumptions:premises ~conclusion
  then Ok ()
  else error "propositional premises do not entail the conclusion"
;;

let check_equality_refl premises conclusion =
  match premises, equality conclusion with
  | [], Some (left, right) when formula_equal left right -> Ok ()
  | _ -> error "equality reflexivity expects no premises and [x = x]"
;;

let check_equality_symm premises conclusion =
  match premises, equality conclusion with
  | [ premise ], Some (left, right) ->
    (match equality premise with
     | Some edge when equality_connects edge left right -> Ok ()
     | None | Some _ -> error "equality symmetry premise does not match")
  | _ -> error "equality symmetry expects one equality premise"
;;

let check_equality_trans premises conclusion =
  match premises, equality conclusion with
  | [ first; second ], Some (left, right) ->
    (match equality first, equality second with
     | Some (a, b), Some (c, d) ->
       let edges = [ a, b; c, d ] in
       let middle_connects x =
         List.exists edges ~f:(fun edge -> equality_connects edge left x)
         && List.exists edges ~f:(fun edge -> equality_connects edge x right)
       in
       if List.exists [ a; b; c; d ] ~f:middle_connects
       then Ok ()
       else error "equality transitivity premises do not form the conclusion"
     | None, _ | _, None ->
       error "equality transitivity premises must be equalities")
  | _ -> error "equality transitivity expects two equality premises"
;;

let check_congruence premises conclusion =
  match equality conclusion with
  | None -> error "congruence conclusion must be an equality"
  | Some (left, right) ->
    if Formula.Op.compare (Formula.op left) (Formula.op right) <> 0
    then error "congruence terms have different heads"
    else (
      match List.zip (Formula.args left) (Formula.args right) with
      | Unequal_lengths -> error "congruence terms have different arities"
      | Ok argument_pairs ->
        let premise_equalities = List.filter_map premises ~f:equality in
        if List.length premise_equalities <> List.length premises
        then error "congruence premises must be equalities"
        else if List.for_all argument_pairs ~f:(fun (left, right) ->
                  formula_equal left right
                  || List.exists premise_equalities ~f:(fun edge ->
                    equality_connects edge left right))
        then Ok ()
        else error "congruence is missing an argument equality")
;;

let rewrite_at_path formula ~path ~from ~to_ =
  let rec rewrite formula = function
    | [] ->
      if formula_equal formula from
      then Ok to_
      else error "rewrite path does not point to the equality's source"
    | index :: path ->
      let args = Formula.args formula in
      if index < 0 || index >= List.length args
      then
        Or_error.error_s
          [%message "rewrite path is out of bounds" (index : int)]
      else (
        let%bind.Or_error rewritten = rewrite (List.nth_exn args index) path in
        let args =
          List.mapi args ~f:(fun i arg -> if i = index then rewritten else arg)
        in
        match Formula.make_opt ~op:(Formula.op formula) ~args with
        | Some formula -> Ok formula
        | None -> error "rewriting produced an ill-shaped formula")
  in
  rewrite formula path
;;

let check_rewrite premises conclusion ~direction ~path =
  match premises with
  | [ equality_premise; source ] ->
    (match equality equality_premise with
     | None -> error "rewrite's first premise must be an equality"
     | Some (left, right) ->
       let from, to_ =
         match direction with
         | Rewrite_direction.Left_to_right -> left, right
         | Right_to_left -> right, left
       in
       let%bind.Or_error rewritten = rewrite_at_path source ~path ~from ~to_ in
       if formula_equal rewritten conclusion
       then Ok ()
       else error "rewrite result does not match its conclusion")
  | _ -> error "rewrite expects an equality and a source premise"
;;

let check_kernel rule premises conclusion =
  match (rule : Kernel_rule.t) with
  | Propositional -> check_propositional premises conclusion
  | Equality_refl -> check_equality_refl premises conclusion
  | Equality_symm -> check_equality_symm premises conclusion
  | Equality_trans -> check_equality_trans premises conclusion
  | Congruence -> check_congruence premises conclusion
  | Rewrite { direction; path } ->
    check_rewrite premises conclusion ~direction ~path
;;

let check proof =
  let%bind.Or_error () =
    Or_error.all_unit
      (Array.to_list proof.assumptions
       |> List.map ~f:(fun assumption ->
         Or_error.map
           (Boolean_formula.of_formula assumption.Assumption.formula)
           ~f:ignore))
  in
  let step_at ~before id =
    let id = Id.Step.to_int id in
    if id < 0 || id >= before
    then
      Or_error.error_s
        [%message "proof step does not refer backwards" (id : int)]
    else Ok proof.steps.(id)
  in
  let%bind.Or_error () =
    Array.foldi proof.steps ~init:(Ok ()) ~f:(fun index result step ->
      let%bind.Or_error () = result in
      let%bind.Or_error () =
        Or_error.map (Boolean_formula.of_formula step.Step.conclusion) ~f:ignore
      in
      match step.justification with
      | Assumption assumption ->
        let assumption = Id.Assumption.to_int assumption in
        if assumption < 0 || assumption >= Array.length proof.assumptions
        then
          Or_error.error_s
            [%message "assumption index out of bounds" (assumption : int)]
        else if formula_equal
                  step.conclusion
                  proof.assumptions.(assumption).Assumption.formula
        then Ok ()
        else error "assumption step conclusion does not match its assumption"
      | Kernel { rule; premises } ->
        let%bind.Or_error premises =
          Or_error.all
            (Array.to_list premises
             |> List.map ~f:(fun premise ->
               Or_error.map (step_at ~before:index premise) ~f:(fun step ->
                 step.Step.conclusion)))
        in
        check_kernel rule premises step.conclusion
      | By_refutation { premises; refutation } ->
        let%bind.Or_error premises =
          Or_error.all
            (Array.to_list premises
             |> List.map ~f:(fun premise ->
               Or_error.map (step_at ~before:index premise) ~f:(fun step ->
                 step.Step.conclusion)))
        in
        let expected_inputs =
          Array.of_list (premises @ [ Formula.Not step.conclusion ])
        in
        if Array.length expected_inputs
           <> Array.length refutation.Refutation.inputs
           || not
                (Array.for_all2_exn
                   expected_inputs
                   refutation.inputs
                   ~f:formula_equal)
        then
          error
            "refutation inputs do not match the cited premises and conclusion"
        else Refutation.check refutation)
  in
  let conclusion = Id.Step.to_int proof.conclusion in
  if conclusion < 0 || conclusion >= Array.length proof.steps
  then
    Or_error.error_s
      [%message "proof conclusion is out of bounds" (conclusion : int)]
  else Ok ()
;;

let check_theory_certificate = Proof_theory_certificate_check.check
