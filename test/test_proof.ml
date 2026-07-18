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
  print_clause
    (Proof.Clause.create
       [ equality_literal ~positive:true; equality_literal ~positive:false ]);
  [%expect
    {|
    (((atom (Theory (Eq ((Var x) (Var y))))) (positive true))
     ((atom (Extension 0)) (positive false)))
    ("round trip" (equal true))
    Tautology
    |}]
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
  print_s
    [%message
      "round trip"
        ~equal:(Proof.compare proof (Proof.t_of_sexp sexp) = 0 : bool)];
  [%expect
    {|
    ((assumptions (((name (h)) (formula (Eq (Var x) (Var x))))))
     (steps
      (((name (same)) (conclusion (Eq (Var x) (Var x)))
        (justification (Assumption 0)))))
     (conclusion 0))
    ("round trip" (equal true))
    |}]
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
  [%expect
    {|
    (checks (refutation true) (proof true))
    ("rejects incomplete RUP" (rejected true))
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
