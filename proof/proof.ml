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
    ; formula : Formula.quantified
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
    | Forall_instantiation of { bound_values : (Tvar.t * Formula.any) list }
    | Exists_elim of { skolems : (Tvar.t * Formula.any) list }
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
    ; conclusion : Formula.quantified
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

let formula_equal_quantified left right =
  Formula.compare_quantified left right = 0
;;

let ground_of_quantified q ~context =
  match Formula.to_any q with
  | Some ground -> Ok ground
  | None -> error context
;;

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

let check_ground_kernel rule premises conclusion =
  match (rule : Kernel_rule.t) with
  | Propositional -> check_propositional premises conclusion
  | Equality_refl -> check_equality_refl premises conclusion
  | Equality_symm -> check_equality_symm premises conclusion
  | Equality_trans -> check_equality_trans premises conclusion
  | Congruence -> check_congruence premises conclusion
  | Rewrite { direction; path } ->
    check_rewrite premises conclusion ~direction ~path
  | Forall_instantiation _ | Exists_elim _ ->
    (* Handled by [check_kernel] before ground conversion. *)
    error "quantifier kernel rule reached the ground checker"
;;

(* Substitutes [bindings] into [body] and checks [conclusion] equals the result,
   requiring exactly the [bound] variables to be bound. Shared by universal
   instantiation and existential elimination. *)
let check_witnessing
  ?(check_binding = fun _ -> Ok ())
  ~bound
  ~body
  ~bindings
  ~conclusion
  ~missing_error
  ()
  =
  match Tvar.Map.of_alist bindings with
  | `Duplicate_key key ->
    Or_error.error_s
      [%message "a bound variable was witnessed twice" (key : Tvar.t)]
  | `Ok subst ->
    let bound_set = Tvar.Set.of_list bound in
    let extra_keys =
      Map.keys subst |> List.filter ~f:(fun key -> not (Set.mem bound_set key))
    in
    if not (List.is_empty extra_keys)
    then
      Or_error.error_s
        [%message
          "a quantifier rule provided witnesses for variables not bound by the \
           premise"
            (extra_keys : Tvar.t list)]
    else if not (List.for_all bound ~f:(Map.mem subst))
    then error missing_error
    else (
      let%bind.Or_error () =
        Or_error.all_unit (List.map bindings ~f:check_binding)
      in
      let%bind.Or_error expected = Formula.substitute_quantified subst body in
      if formula_equal_quantified expected conclusion
      then Ok ()
      else
        error "a quantifier rule's conclusion does not match its witnessed body")
;;

let check_forall_instantiation premises conclusion ~bound_values =
  match premises with
  | [ Formula.Forall (bound, _triggers, body) ] ->
    check_witnessing
      ~bound
      ~body
      ~bindings:bound_values
      ~conclusion
      ~missing_error:
        "universal instantiation must instantiate every bound variable"
      ()
  | [ _ ] -> error "universal instantiation premise must be a [∀]"
  | _ -> error "universal instantiation expects a single premise"
;;

let check_exists_elim premises conclusion ~skolems ~assumption_tvars =
  match premises with
  | [ (Formula.Exists (bound, body) as premise) ] ->
    let%bind.Or_error () =
      check_witnessing
        ~bound
        ~body
        ~bindings:skolems
        ~conclusion
        ~missing_error:
          "existential elimination must witness every bound variable"
        ~check_binding:(fun (bound, witness) ->
          match witness with
          | Formula.Var _ -> Ok ()
          | _ ->
            Or_error.error_s
              [%message
                "existential elimination witness must be a bare fresh variable"
                  (bound : Tvar.t)
                  (witness : Formula.any)])
        ()
    in
    let skolem_tvars_by_witness =
      List.map skolems ~f:(fun (_, witness) ->
        Formula.tvars (Formula.widen_quantified witness))
    in
    let pairwise_disjoint =
      let rec go seen = function
        | [] -> true
        | tvars :: rest ->
          Set.are_disjoint seen tvars && go (Set.union seen tvars) rest
      in
      go Tvar.Set.empty skolem_tvars_by_witness
    in
    if not pairwise_disjoint
    then error "existential elimination's Skolem witnesses must be distinct"
    else (
      let skolem_tvars =
        List.fold skolem_tvars_by_witness ~init:Tvar.Set.empty ~f:Set.union
      in
      let forbidden = Set.union assumption_tvars (Formula.tvars premise) in
      if Set.are_disjoint skolem_tvars forbidden
      then Ok ()
      else
        error
          "existential elimination's Skolem symbol is not fresh (it occurs in \
           the premise or an assumption)")
  | [ _ ] -> error "existential elimination premise must be an [∃]"
  | _ -> error "existential elimination expects a single premise"
;;

let check_kernel ~assumption_tvars rule premises conclusion =
  match (rule : Kernel_rule.t) with
  | Forall_instantiation { bound_values } ->
    check_forall_instantiation premises conclusion ~bound_values
  | Exists_elim { skolems } ->
    check_exists_elim premises conclusion ~skolems ~assumption_tvars
  | Propositional
  | Equality_refl
  | Equality_symm
  | Equality_trans
  | Congruence
  | Rewrite _ ->
    let%bind.Or_error premises =
      Or_error.all
        (List.map premises ~f:(fun premise ->
           ground_of_quantified
             premise
             ~context:"a ground kernel rule was given a quantified premise"))
    in
    let%bind.Or_error conclusion =
      ground_of_quantified
        conclusion
        ~context:"a ground kernel rule concluded a quantified formula"
    in
    check_ground_kernel rule premises conclusion
;;

(* Ground formulas must be well-shaped booleans; quantified ones ([∀]/[∃] and
   the boolean structure over them) are checked structurally by the rules that
   consume them, not by [Boolean_formula.of_formula]. *)
let check_well_formed (q : Formula.quantified) =
  match Formula.to_any q with
  | None -> Ok ()
  | Some ground -> Or_error.map (Boolean_formula.of_formula ground) ~f:ignore
;;

let check proof =
  let assumption_tvars =
    Array.fold proof.assumptions ~init:Tvar.Set.empty ~f:(fun acc assumption ->
      Set.union acc (Formula.tvars assumption.Assumption.formula))
  in
  let%bind.Or_error () =
    Or_error.all_unit
      (Array.to_list proof.assumptions
       |> List.map ~f:(fun assumption ->
         check_well_formed assumption.Assumption.formula))
  in
  let check_skolem_reuse () =
    let skolem_conclusion = Tvar.Table.create () in
    Or_error.all_unit
      (Array.to_list proof.steps
       |> List.map ~f:(fun step ->
         match step.Step.justification with
         | Kernel { rule = Exists_elim { skolems }; _ } ->
           List.fold_until
             skolems
             ~init:()
             ~f:(fun () (_, witness) ->
               let witness_tvars =
                 Formula.tvars (Formula.widen_quantified witness)
               in
               match Set.to_list witness_tvars with
               | [] -> Continue ()
               | tvars ->
                 let duplicate =
                   List.find_map tvars ~f:(fun tvar ->
                     Hashtbl.find skolem_conclusion tvar
                     |> Option.filter ~f:(fun previous ->
                       not (formula_equal_quantified previous step.conclusion))
                     |> Option.map ~f:(fun previous -> tvar, previous))
                 in
                 (match duplicate with
                  | Some (tvar, previous) ->
                    Stop
                      (Or_error.error_s
                         [%message
                           "existential elimination's Skolem symbol was reused \
                            for a different conclusion"
                             (tvar : Tvar.t)
                             (previous : Formula.quantified)
                             ~current:(step.conclusion : Formula.quantified)])
                  | None ->
                    List.iter tvars ~f:(fun tvar ->
                      Hashtbl.set
                        skolem_conclusion
                        ~key:tvar
                        ~data:step.conclusion);
                    Continue ()))
             ~finish:(fun () -> Ok ())
         | Assumption _ | Kernel _ | By_refutation _ -> Ok ()))
  in
  let%bind.Or_error () = check_skolem_reuse () in
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
      let%bind.Or_error () = check_well_formed step.Step.conclusion in
      match step.justification with
      | Assumption assumption ->
        let assumption = Id.Assumption.to_int assumption in
        if assumption < 0 || assumption >= Array.length proof.assumptions
        then
          Or_error.error_s
            [%message "assumption index out of bounds" (assumption : int)]
        else if formula_equal_quantified
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
        check_kernel ~assumption_tvars rule premises step.conclusion
      | By_refutation { premises; refutation } ->
        let%bind.Or_error premises =
          Or_error.all
            (Array.to_list premises
             |> List.map ~f:(fun premise ->
               let%bind.Or_error step = step_at ~before:index premise in
               ground_of_quantified
                 step.Step.conclusion
                 ~context:"a refutation premise must be ground"))
        in
        let%bind.Or_error conclusion =
          ground_of_quantified
            step.conclusion
            ~context:"a refutation's conclusion must be ground"
        in
        let expected_inputs =
          Array.of_list (premises @ [ Formula.Not conclusion ])
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
  else (
    (* The other half of the eigenvariable condition: a Skolem introduced by
       existential elimination names an arbitrary witness, so it must not escape
       into the proof's exported conclusion. (Per-step freshness already keeps
       it out of the assumptions and premise.) In practice conclusions are
       [false], which trivially satisfies this. *)
    let introduced_skolems =
      Array.fold proof.steps ~init:Tvar.Set.empty ~f:(fun acc step ->
        match step.Step.justification with
        | Kernel { rule = Exists_elim { skolems }; _ } ->
          List.fold skolems ~init:acc ~f:(fun acc (_, witness) ->
            Set.union acc (Formula.tvars (Formula.widen_quantified witness)))
        | Assumption _ | Kernel _ | By_refutation _ -> acc)
    in
    let conclusion_tvars =
      Formula.tvars proof.steps.(conclusion).Step.conclusion
    in
    if Set.are_disjoint introduced_skolems conclusion_tvars
    then Ok ()
    else
      error
        "a Skolem introduced by existential elimination escapes into the \
         proof's conclusion (eigenvariable condition)")
;;

let check_theory_certificate ?datatype_env ~clause certificate =
  Proof_theory_certificate_check.check ?datatype_env ~clause certificate
;;

let subst_to_string pairs =
  String.concat
    ~sep:", "
    (List.map pairs ~f:(fun (v, term) ->
       sprintf
         "%s := %s"
         (Tvar.to_string v)
         (Proof_to_string.formula_to_string term)))
;;

let justification_to_string (j : Justification.t) =
  let refs prefix ids =
    Array.to_list ids |> List.map ~f:(fun p -> prefix ^ Int.to_string p)
  in
  let over premises =
    String.concat ~sep:", " (refs "s" (Array.map premises ~f:Id.Step.to_int))
  in
  match j with
  | Assumption id -> sprintf "assumption a%d" (Id.Assumption.to_int id)
  | Kernel { rule = Forall_instantiation { bound_values }; premises } ->
    sprintf
      "∀-instantiation {%s} over [%s]"
      (subst_to_string bound_values)
      (over premises)
  | Kernel { rule = Exists_elim { skolems }; premises } ->
    sprintf
      "∃-elimination {%s} over [%s]"
      (subst_to_string skolems)
      (over premises)
  | Kernel { rule; premises } ->
    sprintf
      "%s over [%s]"
      (Sexp.to_string (Kernel_rule.sexp_of_t rule))
      (over premises)
  | By_refutation { premises; refutation = _ } ->
    sprintf
      "refutation of [%s]"
      (String.concat
         ~sep:", "
         (refs "s" (Array.map premises ~f:Id.Step.to_int)))
;;

let to_string_hum (proof : t) =
  let out = Proof_to_string.Buffer_out.create () in
  let open Proof_to_string.Buffer_out in
  line out "Assumptions:";
  indented out ~f:(fun () ->
    Array.iteri proof.assumptions ~f:(fun index assumption ->
      line
        out
        (sprintf
           "a%d: %s"
           index
           (Proof_to_string.quantified_to_string assumption.Assumption.formula))));
  line out "Steps:";
  indented out ~f:(fun () ->
    Array.iteri proof.steps ~f:(fun index step ->
      line
        out
        (sprintf
           "s%d: %s   [%s]"
           index
           (Proof_to_string.quantified_to_string step.Step.conclusion)
           (justification_to_string step.justification));
      match step.justification with
      | By_refutation { refutation; premises } ->
        indented out ~f:(fun () ->
          Proof_to_string.render_refutation
            out
            ~premise_steps:(Array.map premises ~f:Id.Step.to_int)
            refutation)
      | Assumption _ | Kernel _ -> ()));
  line out (sprintf "Conclusion: s%d" (Id.Step.to_int proof.conclusion));
  contents out
;;
