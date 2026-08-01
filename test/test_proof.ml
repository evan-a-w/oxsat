open! Core
open! Feel.Import
open! Theory_core
open! Theory

let x = Tvar.of_string "x"
let y = Tvar.of_string "y"

let forall (bound, triggers, body) : Formula.quantified =
  Forall
    ( bound
    , List.map triggers ~f:(List.map ~f:Formula.widen_quantified)
    , Formula.widen_quantified body )
;;

let exists (bound, body) : Formula.quantified =
  Exists (bound, Formula.widen_quantified body)
;;

let equality_literal ~positive =
  Proof.Literal.create
    ~atom:(Proof.Atom.Theory (`Eq (Formula.Var y, Formula.Var x)))
    ~positive
;;

let print_clause = function
  | `Tautology -> print_endline "Tautology"
  | `Clause clause -> print_s [%sexp (clause : Proof.Clause.t)]
;;

let clause_exn literals =
  match Proof.Clause.create literals with
  | `Clause clause -> clause
  | `Tautology -> failwith "unexpected tautology"
;;

let%expect_test "proof clauses normalize, sort, and deduplicate literals" =
  let extension =
    Proof.Literal.create
      ~atom:(Proof.Atom.Extension (Proof.Id.Extension.of_int_exn 0))
      ~positive:false
  in
  let clause =
    Proof.Clause.create
      [ equality_literal ~positive:true
      ; extension
      ; equality_literal ~positive:true
      ]
  in
  print_clause clause;
  [%expect
    {|
    (((atom (Theory (Eq ((Var x) (Var y))))) (positive true))
     ((atom (Extension 0)) (positive false)))
    |}];
  (match clause with
   | `Tautology -> assert false
   | `Clause clause ->
     let sexp = Proof.Clause.sexp_of_t clause in
     print_s
       [%message
         "round trip"
           ~equal:
             (Proof.Clause.compare clause (Proof.Clause.t_of_sexp sexp) = 0
              : bool)]);
  [%expect {| ("round trip" (equal true)) |}];
  print_clause
    (Proof.Clause.create
       [ equality_literal ~positive:true; equality_literal ~positive:false ]);
  [%expect {| Tautology |}]
;;

let%expect_test "manual proofs have a stable S-expression representation" =
  let formula : Formula.quantified = Eq (Var x, Var x) in
  let proof : Proof.t =
    { assumptions = [| { name = Some "h"; formula } |]
    ; steps =
        [| { name = Some "same"
           ; conclusion = formula
           ; justification = Assumption (Proof.Id.Assumption.of_int_exn 0)
           }
        |]
    ; conclusion = Proof.Id.Step.of_int_exn 0
    }
  in
  let sexp = Proof.sexp_of_t proof in
  print_s sexp;
  [%expect
    {|
    ((assumptions (((name (h)) (formula (Eq (Var x) (Var x))))))
     (steps
      (((name (same)) (conclusion (Eq (Var x) (Var x)))
        (justification (Assumption 0)))))
     (conclusion 0))
    |}];
  print_s
    [%message
      "round trip"
        ~equal:(Proof.compare proof (Proof.t_of_sexp sexp) = 0 : bool)];
  [%expect {| ("round trip" (equal true)) |}]
;;

let refutation_of_false_input () =
  let extension_id = Proof.Id.Extension.of_int_exn 0 in
  let extension_atom = Proof.Atom.Extension extension_id in
  let positive = Proof.Literal.create ~atom:extension_atom ~positive:true in
  let negative = Proof.Literal.neg positive in
  let step_id i = Proof.Id.Refutation_step.of_int_exn i in
  let steps : Proof.Refutation.Step.t array =
    [| { clause = clause_exn [ positive ]
       ; reason = Input_clause { input = 0; literal = positive }
       }
     ; { clause = clause_exn [ negative ]
       ; reason = Extension_definition extension_id
       }
     ; { clause = Proof.Clause.empty
       ; reason = Rup { hints = [| step_id 0; step_id 1 |] }
       }
    |]
  in
  { Proof.Refutation.inputs = [| Formula.Not Formula.True |]
  ; extensions =
      [| { Proof.Extension.id = extension_id; definition = Proof.Boolean.False }
      |]
  ; datatype_env = Datatype.Env.empty
  ; steps
  ; contradiction = step_id 2
  }
;;

let%expect_test "the independent checker accepts a manual by-refutation step" =
  let refutation = refutation_of_false_input () in
  let proof : Proof.t =
    { assumptions = [||]
    ; steps =
        [| { name = Some "truth"
           ; conclusion = Formula.True
           ; justification = By_refutation { premises = [||]; refutation }
           }
        |]
    ; conclusion = Proof.Id.Step.of_int_exn 0
    }
  in
  print_s
    [%message
      "checks"
        ~refutation:(Or_error.is_ok (Proof.Refutation.check refutation) : bool)
        ~proof:(Or_error.is_ok (Proof.check proof) : bool)];
  [%expect {| (checks (refutation true) (proof true)) |}];
  let invalid_refutation =
    { refutation with
      steps =
        Array.mapi refutation.steps ~f:(fun index step ->
          if index = 2
          then
            { step with
              reason =
                Rup { hints = [| Proof.Id.Refutation_step.of_int_exn 0 |] }
            }
          else step)
    }
  in
  let invalid_proof : Proof.t =
    { proof with
      steps =
        [| { (proof.steps.(0)) with
             justification =
               By_refutation
                 { premises = [||]; refutation = invalid_refutation }
           }
        |]
    }
  in
  print_s
    [%message
      "rejects incomplete RUP"
        ~refutation_rejected:
          (Or_error.is_error (Proof.Refutation.check invalid_refutation) : bool)
        ~proof_rejected:(Or_error.is_error (Proof.check invalid_proof) : bool)];
  print_s [%sexp (Proof.check invalid_proof : unit Or_error.t)];
  [%expect
    {|
    ("rejects incomplete RUP" (refutation_rejected true) (proof_rejected true))
    (Error "RUP hints did not derive a conflict")
    |}]
;;

let%expect_test "RUP propagates through input and extension clauses" =
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  let c : Formula.any = Var (Tvar.of_string "c") in
  let d : Formula.any = Var (Tvar.of_string "d") in
  let p : Formula.any = Eq (a, b) in
  let q : Formula.any = Eq (c, d) in
  let p_atom = Proof.Atom.Theory (`Eq (a, b)) in
  let q_atom = Proof.Atom.Theory (`Eq (c, d)) in
  let literal atom ~positive = Proof.Literal.create ~atom ~positive in
  let p_positive = literal p_atom ~positive:true in
  let p_negative = literal p_atom ~positive:false in
  let q_positive = literal q_atom ~positive:true in
  let q_negative = literal q_atom ~positive:false in
  let extension_id i = Proof.Id.Extension.of_int_exn i in
  let extension_literal i ~positive =
    literal (Proof.Atom.Extension (extension_id i)) ~positive
  in
  let extension i definition : Proof.Extension.t =
    { id = extension_id i; definition }
  in
  let input_step input =
    let literal = extension_literal input ~positive:true in
    ({ clause = clause_exn [ literal ]
     ; reason = Input_clause { input; literal }
     }
     : Proof.Refutation.Step.t)
  in
  let extension_step id literals : Proof.Refutation.Step.t =
    { clause = clause_exn literals
    ; reason = Extension_definition (extension_id id)
    }
  in
  let step_id i = Proof.Id.Refutation_step.of_int_exn i in
  let steps : Proof.Refutation.Step.t array =
    [| input_step 0
     ; extension_step 0 [ extension_literal 0 ~positive:false; p_positive ]
     ; input_step 1
     ; extension_step
         1
         [ extension_literal 1 ~positive:false; p_negative; q_positive ]
     ; input_step 2
     ; extension_step 2 [ extension_literal 2 ~positive:false; q_negative ]
     ; { clause = Proof.Clause.empty
       ; reason =
           Rup
             { hints =
                 [| step_id 0
                  ; step_id 1
                  ; step_id 2
                  ; step_id 3
                  ; step_id 4
                  ; step_id 5
                 |]
             }
       }
    |]
  in
  let refutation : Proof.Refutation.t =
    { inputs = [| p; Or [ Not p; q ]; Not q |]
    ; extensions =
        [| extension 0 (Atom p_atom)
         ; extension 1 (Or [ Not (Atom p_atom); Atom q_atom ])
         ; extension 2 (Not (Atom q_atom))
        |]
    ; datatype_env = Datatype.Env.empty
    ; steps
    ; contradiction = step_id 6
    }
  in
  let badly_ordered_hints =
    { refutation with
      steps =
        Array.mapi refutation.steps ~f:(fun index step ->
          if index = 6
          then
            { step with
              reason =
                Rup
                  { hints =
                      [| step_id 0
                       ; step_id 1
                       ; step_id 3
                       ; step_id 2
                       ; step_id 4
                       ; step_id 5
                      |]
                  }
            }
          else step)
    }
  in
  print_s
    [%message
      "checks"
        ~propagation_chain:
          (Or_error.is_ok (Proof.Refutation.check refutation) : bool)
        ~bad_hint_order_rejected:
          (Or_error.is_error (Proof.Refutation.check badly_ordered_hints)
           : bool)];
  [%expect
    {|
    (checks (propagation_chain true) (bad_hint_order_rejected true))
    |}]
;;

let%expect_test "kernel equality transitivity is checked" =
  let a : Formula.quantified = Var (Tvar.of_string "a") in
  let b : Formula.quantified = Var (Tvar.of_string "b") in
  let c : Formula.quantified = Var (Tvar.of_string "c") in
  let assumptions : Proof.Assumption.t array =
    [| { name = None; formula = Eq (a, b) }
     ; { name = None; formula = Eq (b, c) }
    |]
  in
  let step_id i = Proof.Id.Step.of_int_exn i in
  let proof : Proof.t =
    { assumptions
    ; steps =
        [| { name = None
           ; conclusion = Eq (a, b)
           ; justification = Assumption (Proof.Id.Assumption.of_int_exn 0)
           }
         ; { name = None
           ; conclusion = Eq (b, c)
           ; justification = Assumption (Proof.Id.Assumption.of_int_exn 1)
           }
         ; { name = None
           ; conclusion = Eq (a, c)
           ; justification =
               Kernel
                 { rule = Equality_trans
                 ; premises = [| step_id 0; step_id 1 |]
                 }
           }
        |]
    ; conclusion = step_id 2
    }
  in
  print_s [%message "checks" ~valid:(Or_error.is_ok (Proof.check proof) : bool)];
  [%expect {| (checks (valid true)) |}]
;;

let%expect_test "kernel universal instantiation is checked" =
  let x = Tvar.of_string "x" in
  let f = Tvar.of_string "f" in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  let forall : Formula.quantified =
    forall ([ x ], [], Eq (App (f, [ Var x ]), Var x))
  in
  let instance : Formula.quantified =
    Formula.widen_quantified (Formula.Eq (App (f, [ a ]), a))
  in
  (* A non-[∀] premise, to check the rule rejects being applied to it. *)
  let not_a_forall : Formula.quantified =
    exists ([ x ], Eq (App (f, [ Var x ]), Var x))
  in
  let proof ?(premise = forall) rule : Proof.t =
    { assumptions = [| { name = None; formula = premise } |]
    ; steps =
        [| { name = None
           ; conclusion = premise
           ; justification = Assumption (Proof.Id.Assumption.of_int_exn 0)
           }
         ; { name = None
           ; conclusion = instance
           ; justification =
               Kernel { rule; premises = [| Proof.Id.Step.of_int_exn 0 |] }
           }
        |]
    ; conclusion = Proof.Id.Step.of_int_exn 1
    }
  in
  let valid = proof (Forall_instantiation { bound_values = [ x, a ] }) in
  let rejects rule ~premise =
    Or_error.is_error (Proof.check (proof ~premise rule))
  in
  print_s
    [%message
      ""
        ~valid:(Or_error.is_ok (Proof.check valid) : bool)
        ~wrong_substitution_rejected:
          (rejects
             (Forall_instantiation { bound_values = [ x, b ] })
             ~premise:forall
           : bool)
        ~missing_variable_rejected:
          (rejects (Forall_instantiation { bound_values = [] }) ~premise:forall
           : bool)
        ~non_forall_premise_rejected:
          (rejects
             (Forall_instantiation { bound_values = [ x, a ] })
             ~premise:not_a_forall
           : bool)];
  print_endline (Proof.to_string_hum valid);
  [%expect
    {|
    ((valid true) (wrong_substitution_rejected true)
     (missing_variable_rejected true) (non_forall_premise_rejected true))
    Assumptions:
      a0: ∀x. f(x) = x
    Steps:
      s0: ∀x. f(x) = x   [assumption a0]
      s1: f(a) = a   [∀-instantiation {x := a} over [s0]]
    Conclusion: s1
    |}]
;;

let%expect_test "universal instantiation rejects witnesses for unbound \
                 variables"
  =
  let x = Tvar.of_string "x" in
  let f = Tvar.of_string "f" in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  let c = Tvar.of_string "c" in
  let premise : Formula.quantified =
    forall ([ x ], [], Eq (App (f, [ Var x ]), Var c))
  in
  let step_id = Proof.Id.Step.of_int_exn in
  let assumption_id = Proof.Id.Assumption.of_int_exn in
  let proof ~conclusion ~bound_values : Proof.t =
    { assumptions = [| { name = Some "a0"; formula = premise } |]
    ; steps =
        [| { name = Some "s0"
           ; conclusion = premise
           ; justification = Assumption (assumption_id 0)
           }
         ; { name = Some "s1"
           ; conclusion
           ; justification =
               Kernel
                 { rule = Forall_instantiation { bound_values }
                 ; premises = [| step_id 0 |]
                 }
           }
        |]
    ; conclusion = step_id 1
    }
  in
  let bogus =
    proof
      ~conclusion:(Formula.widen_quantified (Eq (App (f, [ a ]), b)))
      ~bound_values:[ x, a; c, b ]
  in
  let valid =
    proof
      ~conclusion:(Formula.widen_quantified (Eq (App (f, [ a ]), Var c)))
      ~bound_values:[ x, a ]
  in
  print_s [%message "bogus" ~result:(Proof.check bogus : unit Or_error.t)];
  print_s [%message "valid" ~result:(Proof.check valid : unit Or_error.t)];
  [%expect
    {|
    (bogus
     (result
      (Error
       ("a quantifier rule provided witnesses for variables not bound by the premise"
        (extra_keys (c))))))
    (valid (result (Ok ())))
    |}]
;;

let%expect_test "existential elimination rejects non-variable witnesses" =
  let x = Tvar.of_string "x" in
  let sk : Formula.any = Var (Tvar.of_string "%sk") in
  let zero : Formula.any = La_const Q.zero in
  let positive term : Formula.quantified =
    Formula.widen_quantified (La_compare (term, `Gt, zero))
  in
  let premise : Formula.quantified = exists ([ x ], positive (Var x)) in
  let step_id = Proof.Id.Step.of_int_exn in
  let assumption_id = Proof.Id.Assumption.of_int_exn in
  let assumption_step : Proof.Step.t =
    { name = Some "s0"
    ; conclusion = premise
    ; justification = Assumption (assumption_id 0)
    }
  in
  let exists_step ~witness ~witnessed : Proof.Step.t =
    { name = Some "s1"
    ; conclusion = witnessed
    ; justification =
        Kernel
          { rule = Exists_elim { skolems = [ x, witness ] }
          ; premises = [| step_id 0 |]
          }
    }
  in
  let proof ~steps ~conclusion : Proof.t =
    { assumptions = [| { name = Some "a0"; formula = premise } |]
    ; steps
    ; conclusion
    }
  in
  let bogus =
    proof
      ~steps:
        [| assumption_step
         ; exists_step ~witness:zero ~witnessed:(positive zero)
        |]
      ~conclusion:(step_id 1)
  in
  let valid_final : Proof.Step.t =
    { name = Some "true"
    ; conclusion = True
    ; justification =
        Kernel { rule = Propositional; premises = [| step_id 1 |] }
    }
  in
  let valid =
    proof
      ~steps:
        [| assumption_step
         ; exists_step ~witness:sk ~witnessed:(positive sk)
         ; valid_final
        |]
      ~conclusion:(step_id 2)
  in
  print_s [%message "bogus" ~result:(Proof.check bogus : unit Or_error.t)];
  print_s [%message "valid" ~result:(Proof.check valid : unit Or_error.t)];
  [%expect
    {|
    (bogus
     (result
      (Error
       ("existential elimination witness must be a bare fresh variable" (bound x)
        (witness (La_const ((num 0) (den 1))))))))
    (valid (result (Ok ())))
    |}]
;;

let%expect_test "kernel existential elimination is checked, with a freshness \
                 side condition"
  =
  let x = Tvar.of_string "x" in
  let f = Tvar.of_string "f" in
  let sk : Formula.any = Var (Tvar.of_string "%sk") in
  let existential : Formula.quantified =
    exists ([ x ], Not (Eq (App (f, [ Var x ]), Var x)))
  in
  let witnessed : Formula.quantified =
    Formula.widen_quantified (Formula.Not (Eq (App (f, [ sk ]), sk)))
  in
  (* A non-[∃] premise, to check the rule rejects being applied to it. *)
  let not_an_exists : Formula.quantified =
    forall ([ x ], [], Not (Eq (App (f, [ Var x ]), Var x)))
  in
  let proof
    ?(premise = existential)
    ?(conclusion = witnessed)
    ~extra_assumptions
    ()
    : Proof.t
    =
    let assumptions =
      Array.of_list
        ({ Proof.Assumption.name = None; formula = premise }
         :: extra_assumptions)
    in
    { assumptions
    ; steps =
        [| { name = None
           ; conclusion = premise
           ; justification = Assumption (Proof.Id.Assumption.of_int_exn 0)
           }
         ; { name = None
           ; conclusion
           ; justification =
               Kernel
                 { rule = Exists_elim { skolems = [ x, sk ] }
                 ; premises = [| Proof.Id.Step.of_int_exn 0 |]
                 }
           }
        |]
    ; conclusion = Proof.Id.Step.of_int_exn 1
    }
  in
  (* Regression: a bare [∃-elimination] whose conclusion is the witnessed body
     is NOT a valid standalone proof -- [f(%sk) ≠ %sk] does not follow from
     [∃x. f(x) ≠ x] ([%sk] names an arbitrary witness). The step itself is
     locally well-formed, so this is only caught by the eigenvariable escape
     check on the proof's conclusion. *)
  let eigenvariable_escape = proof ~extra_assumptions:[] () in
  (* The Skolem [%sk] occurs in another assumption -- eigenvariable condition
     violated. *)
  let not_fresh =
    proof
      ~extra_assumptions:
        [ { Proof.Assumption.name = None
          ; formula =
              Formula.widen_quantified
                (Formula.Eq (sk, Var (Tvar.of_string "c")))
          }
        ]
      ()
  in
  (* Conclusion witnesses [x := c] but the rule's skolems say [x := %sk]. *)
  let wrong_witness =
    proof
      ~conclusion:
        (Formula.widen_quantified
           (Formula.Not
              (Eq
                 ( App (f, [ Var (Tvar.of_string "c") ])
                 , Var (Tvar.of_string "c") ))))
      ~extra_assumptions:[]
      ()
  in
  let non_exists_premise =
    proof ~premise:not_an_exists ~extra_assumptions:[] ()
  in
  print_s
    [%message
      ""
        ~eigenvariable_escape_rejected:
          (Or_error.is_error (Proof.check eigenvariable_escape) : bool)
        ~stale_skolem_rejected:
          (Or_error.is_error (Proof.check not_fresh) : bool)
        ~wrong_witness_rejected:
          (Or_error.is_error (Proof.check wrong_witness) : bool)
        ~non_exists_premise_rejected:
          (Or_error.is_error (Proof.check non_exists_premise) : bool)];
  print_endline
    "rejected standalone existential elimination (eigenvariable escapes):";
  print_endline (Proof.to_string_hum eigenvariable_escape);
  print_s [%sexp (Proof.check eigenvariable_escape : unit Or_error.t)];
  [%expect
    {|
    ((eigenvariable_escape_rejected true) (stale_skolem_rejected true)
     (wrong_witness_rejected true) (non_exists_premise_rejected true))
    rejected standalone existential elimination (eigenvariable escapes):
    Assumptions:
      a0: ∃x. f(x) ≠ x
    Steps:
      s0: ∃x. f(x) ≠ x   [assumption a0]
      s1: f(%sk) ≠ %sk   [∃-elimination {x := %sk} over [s0]]
    Conclusion: s1

    (Error
     "a Skolem introduced by existential elimination escapes into the proof's conclusion (eigenvariable condition)")
    |}]
;;

let%expect_test "existential elimination rejects capture by an inner binder" =
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let p = Tvar.of_string "p" in
  let premise : Formula.quantified =
    Exists
      ( [ x ]
      , Forall ([ y ], [], Formula.widen_quantified (App (p, [ Var x; Var y ])))
      )
  in
  let captured : Formula.quantified =
    Forall ([ y ], [], Formula.widen_quantified (App (p, [ Var y; Var y ])))
  in
  let proof : Proof.t =
    { assumptions = [| { name = None; formula = premise } |]
    ; steps =
        [| { name = None
           ; conclusion = premise
           ; justification = Assumption (Proof.Id.Assumption.of_int_exn 0)
           }
         ; { name = None
           ; conclusion = captured
           ; justification =
               Kernel
                 { rule = Exists_elim { skolems = [ x, Var y ] }
                 ; premises = [| Proof.Id.Step.of_int_exn 0 |]
                 }
           }
        |]
    ; conclusion = Proof.Id.Step.of_int_exn 1
    }
  in
  print_s [%sexp (Proof.check proof : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("substitution would capture an inner universal binder" (captured (y))))
    |}]
;;

let%expect_test "a multi-rule proof DAG is checked" =
  let a : Formula.quantified = Var (Tvar.of_string "a") in
  let b : Formula.quantified = Var (Tvar.of_string "b") in
  let c : Formula.quantified = Var (Tvar.of_string "c") in
  let d : Formula.quantified = Var (Tvar.of_string "d") in
  let f = Tvar.of_string "f" in
  let app argument : Formula.quantified = App (f, [ argument ]) in
  let source : Formula.quantified = Not (Eq (app a, d)) in
  let rewritten : Formula.quantified = Not (Eq (app c, d)) in
  let congruence : Formula.quantified = Eq (app a, app c) in
  let assumptions : Proof.Assumption.t array =
    [| { name = Some "ab"; formula = Eq (a, b) }
     ; { name = Some "bc"; formula = Eq (b, c) }
     ; { name = Some "source"; formula = source }
    |]
  in
  let step_id i = Proof.Id.Step.of_int_exn i in
  let assumption_id i = Proof.Id.Assumption.of_int_exn i in
  let proof : Proof.t =
    { assumptions
    ; steps =
        [| { name = None
           ; conclusion = Eq (a, b)
           ; justification = Assumption (assumption_id 0)
           }
         ; { name = None
           ; conclusion = Eq (b, c)
           ; justification = Assumption (assumption_id 1)
           }
         ; { name = Some "ac"
           ; conclusion = Eq (a, c)
           ; justification =
               Kernel
                 { rule = Equality_trans
                 ; premises = [| step_id 0; step_id 1 |]
                 }
           }
         ; { name = None
           ; conclusion = source
           ; justification = Assumption (assumption_id 2)
           }
         ; { name = Some "f_a_f_c"
           ; conclusion = congruence
           ; justification =
               Kernel { rule = Congruence; premises = [| step_id 2 |] }
           }
         ; { name = Some "rewritten"
           ; conclusion = rewritten
           ; justification =
               Kernel
                 { rule =
                     Rewrite { direction = Left_to_right; path = [ 0; 0; 0 ] }
                 ; premises = [| step_id 2; step_id 3 |]
                 }
           }
         ; { name = Some "combined"
           ; conclusion = And [ congruence; rewritten ]
           ; justification =
               Kernel
                 { rule = Propositional; premises = [| step_id 4; step_id 5 |] }
           }
        |]
    ; conclusion = step_id 6
    }
  in
  let bad_rewrite =
    { proof with
      steps =
        Array.mapi proof.steps ~f:(fun index step ->
          if index = 5
          then
            { step with
              justification =
                Kernel
                  { rule =
                      Rewrite { direction = Left_to_right; path = [ 0; 0 ] }
                  ; premises = [| step_id 2; step_id 3 |]
                  }
            }
          else step)
    }
  in
  print_s
    [%message
      "checks"
        ~valid:(Or_error.is_ok (Proof.check proof) : bool)
        ~bad_rewrite_rejected:
          (Or_error.is_error (Proof.check bad_rewrite) : bool)];
  [%expect {| (checks (valid true) (bad_rewrite_rejected true)) |}]
;;

let%expect_test "a solver refutation proof prints as human-readable text" =
  let solver =
    Solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  let c : Formula.any = Var (Tvar.of_string "c") in
  let d : Formula.any = Var (Tvar.of_string "d") in
  let assert_ok formula =
    match Or_error.ok_exn (Solver.assert_formula solver formula) with
    | `Ok -> ()
    | `Unsat _ -> print_endline "unsat at assert time"
  in
  assert_ok (Or [ Eq (a, b); Eq (c, d) ]);
  assert_ok (Not (Eq (a, b)));
  assert_ok (Not (Eq (c, d)));
  (match Solver.solve solver with
   | Sat _ -> print_endline "sat"
   | Unsat { proof = Some proof; _ } ->
     print_endline (Proof.to_string_hum proof);
     print_s [%message "check" ~result:(Proof.check proof : unit Or_error.t)]
   | Unsat { proof = None; _ } -> print_endline "no proof produced");
  [%expect
    {|
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: a = b ∨ c = d
      a4: a ≠ b
      a5: c ≠ d
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: a = b ∨ c = d   [assumption a3]
      s4: a ≠ b   [assumption a4]
      s5: c ≠ d   [assumption a5]
      s6: false   [refutation of [s0, s1, s2, s3, s4, s5]]
        refutation:
          extensions:
            e0 := (a = b ∨ c = d)
          steps:
            r0: a = b ∨ c = d ∨ ¬(e0)   [definition of e0]
            r1: e0   [s3]
            r2: a ≠ b   [s4]
            r3: c ≠ d   [s5]
            r4: ⊥   [RUP over [r1, r2, r3, r0]]
    Conclusion: s6

    (check (result (Ok ())))
    |}]
;;

let%expect_test "bare equality bridge and integer split certificates are \
                 checked"
  =
  let theory_literal atom ~positive =
    Proof.Literal.create ~atom:(Proof.Atom.Theory atom) ~positive
  in
  let bare_clause =
    clause_exn
      [ theory_literal
          (`Type_eq (Type_expr.Var x, Type_expr.Var y))
          ~positive:true
      ; theory_literal (`Eq (Formula.Var x, Formula.Var y)) ~positive:false
      ]
  in
  let integer_clause =
    clause_exn
      [ theory_literal
          (`Type_eq (Type_expr.Var x, Type_expr.Base Int))
          ~positive:false
      ; theory_literal (`Le (Linear_expr.var x, Q.one)) ~positive:true
      ; theory_literal
          (`Le (Linear_expr.neg (Linear_expr.var x), Q.of_int (-2)))
          ~positive:true
      ]
  in
  let check clause certificate =
    Or_error.is_ok (Proof.check_theory_certificate ~clause certificate)
  in
  print_s
    [%message
      "certificates"
        ~bare:
          (check
             bare_clause
             (Bare_var_eq (Equality_implies_type_equality (x, y)))
           : bool)
        ~integer:
          (check
             integer_clause
             (Integer_split { variable = x; floor = Q.one; ceil = Q.of_int 2 })
           : bool)
        ~bad_integer:
          (check
             integer_clause
             (Integer_split { variable = x; floor = Q.one; ceil = Q.of_int 3 })
           : bool)];
  [%expect {| (certificates (bare true) (integer true) (bad_integer false)) |}]
;;
