open! Core
open! Feel.Import
open! Theory_core
open! Theory

let x = Tvar.of_string "x"
let y = Tvar.of_string "y"

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
  let formula : Formula.any = Eq (Var x, Var x) in
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
  print_s
    [%message
      "rejects incomplete RUP"
        ~rejected:
          (Or_error.is_error (Proof.Refutation.check invalid_refutation) : bool)];
  [%expect {| ("rejects incomplete RUP" (rejected true)) |}]
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
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  let c : Formula.any = Var (Tvar.of_string "c") in
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

let%expect_test "a multi-rule proof DAG is checked" =
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  let c : Formula.any = Var (Tvar.of_string "c") in
  let d : Formula.any = Var (Tvar.of_string "d") in
  let f = Tvar.of_string "f" in
  let app argument : Formula.any = App (f, [ argument ]) in
  let source : Formula.any = Not (Eq (app a, d)) in
  let rewritten : Formula.any = Not (Eq (app c, d)) in
  let congruence : Formula.any = Eq (app a, app c) in
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
