open! Core
open! Feel.Import
open! Theory_core
open! Theory

let v name : Formula.any = Var (Tvar.of_string name)
let true_ : Formula.any = True
let false_ : Formula.any = False
let eq left right : Formula.any = Eq (left, right)
let neq left right : Formula.any = Not (eq left right)
let ite condition then_ else_ : Formula.any = Ite (condition, then_, else_)
let app f args : Formula.any = App (Tvar.of_string f, args)
let select array index : Formula.any = Select (array, index)
let store array index value : Formula.any = Store (array, index, value)

let assert_ok solver formula =
  match Or_error.ok_exn (Solver.assert_formula solver formula) with
  | `Ok -> ()
  | `Unsat _ -> print_endline "Unsat at assert time"
;;

let assert_q_ok solver formula =
  match Or_error.ok_exn (Quantifier_solver.assert_formula solver formula) with
  | `Ok -> ()
  | `Unsat _ -> print_endline "Unsat at assert time"
;;

let print_solver_result = function
  | Solver_result.Unsat _ -> print_endline "Unsat"
  | Sat _ -> print_endline "Sat"
;;

let assert_and_print_result solver formula =
  match Or_error.ok_exn (Solver.assert_formula solver formula) with
  | `Unsat _ -> print_endline "Unsat"
  | `Ok -> print_solver_result (Solver.solve solver)
;;

let print_quantifier_result = function
  | Quantifier_solver.Result.Unsat _ -> print_endline "Unsat"
  | Sat _ -> print_endline "Sat"
  | Unknown_but_possibly_sat _ -> print_endline "Unknown"
;;

let print_proof_result = function
  | Solver_result.Sat _ -> print_endline "Sat"
  | Unsat { proof = Some proof; _ } ->
    print_s
      [%message "Unsat" ~proof_check:(Proof.check proof : unit Or_error.t)]
  | Unsat { proof = None; _ } -> print_endline "Unsat without proof"
;;

let%expect_test "ground term ite chooses true branch in equality" =
  let solver = Solver.create () in
  let condition = eq (v "c1") (v "c2") in
  assert_and_print_result
    solver
    (And
       [ condition
       ; eq (v "x") (v "z")
       ; neq (ite condition (v "x") (v "y")) (v "z")
       ]);
  [%expect {| Unsat |}]
;;

let%expect_test "ground term ite chooses false branch in equality" =
  let solver = Solver.create () in
  let condition = eq (v "c1") (v "c2") in
  assert_and_print_result
    solver
    (And
       [ Not condition
       ; eq (v "y") (v "z")
       ; neq (ite condition (v "x") (v "y")) (v "z")
       ]);
  [%expect {| Unsat |}]
;;

let%expect_test "ite as a UF argument participates in congruence" =
  let solver = Solver.create () in
  let condition = eq (v "c1") (v "c2") in
  assert_ok solver condition;
  assert_ok
    solver
    (neq (app "f" [ ite condition (v "x") (v "y") ]) (app "f" [ v "x" ]));
  print_solver_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "ite as an array term selects the chosen array" =
  let solver = Solver.create () in
  let condition = eq (v "c1") (v "c2") in
  assert_ok solver condition;
  assert_ok
    solver
    (neq
       (select (ite condition (v "a") (v "b")) (v "i"))
       (select (v "a") (v "i")));
  print_solver_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "ite under not and or follows the selected branch" =
  let condition = eq (v "c1") (v "c2") in
  let solver = Solver.create () in
  assert_ok
    solver
    (Not (ite condition (eq (v "x") (v "y")) (eq (v "y") (v "z"))));
  assert_ok solver condition;
  assert_ok solver (eq (v "x") (v "y"));
  print_solver_result (Solver.solve solver);
  let solver = Solver.create () in
  assert_ok
    solver
    (Or
       [ ite condition (eq (v "x") (v "y")) (eq (v "y") (v "z"))
       ; eq (v "u") (v "w")
       ]);
  assert_ok solver condition;
  assert_ok solver (neq (v "x") (v "y"));
  assert_ok solver (neq (v "u") (v "w"));
  print_solver_result (Solver.solve solver);
  [%expect {|
    Unsat
    Unsat
    |}]
;;

let%expect_test "same-condition ite equality respects branch constraints" =
  let condition = eq (v "c1") (v "c2") in
  let solver = Solver.create () in
  assert_ok solver condition;
  assert_ok solver (neq (v "x") (v "z"));
  (match
     Or_error.ok_exn
       (Solver.assert_formula
          solver
          (eq (ite condition (v "x") (v "y")) (ite condition (v "z") (v "y"))))
   with
   | `Unsat _ -> print_endline "Unsat"
   | `Ok -> print_solver_result (Solver.solve solver));
  [%expect {| Unsat |}]
;;

let%expect_test "ite in store still satisfies read-over-write" =
  let condition = eq (v "c1") (v "c2") in
  let solver = Solver.create () in
  assert_ok
    solver
    (neq
       (select
          (store (ite condition (v "a") (v "b")) (v "i") (v "value"))
          (v "i"))
       (v "value"));
  print_solver_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "scoped boolean contradiction can be popped" =
  let solver = Solver.create () in
  let p = eq (v "p1") (v "p2") in
  let q = eq (v "q1") (v "q2") in
  Solver.push solver;
  assert_ok solver (Or [ p; q ]);
  assert_ok solver (Or [ Not p; q ]);
  assert_ok solver (Or [ p; Not q ]);
  assert_ok solver (Or [ Not p; Not q ]);
  print_solver_result (Solver.solve solver);
  Solver.pop solver;
  print_solver_result (Solver.solve solver);
  [%expect {|
    Unsat
    Sat
    |}]
;;

let%expect_test "ite under a quantifier body is lowered after instantiation" =
  let solver = Quantifier_solver.create () in
  let x = Tvar.of_string "x" in
  let y = v "y" in
  let body = eq (app "f" [ ite (eq (Var x) (Var x)) (Var x) y ]) (Var x) in
  assert_q_ok solver (Forall ([ x ], [ [ app "f" [ Var x ] ] ], body));
  assert_q_ok
    solver
    (Formula.widen_quantified (neq (app "f" [ v "a" ]) (v "a")));
  print_quantifier_result (Quantifier_solver.solve solver ~max_rounds:4);
  [%expect {| Unsat |}]
;;

let%expect_test "satisfiable ite problem has a checkable model" =
  let solver = Solver.create () in
  assert_ok solver (eq (ite false_ (v "x") (v "y")) (v "z"));
  assert_ok solver (eq (v "y") (v "z"));
  assert_ok solver (neq (v "x") (v "z"));
  (match Solver.solve solver with
   | Unsat _ -> print_endline "Unsat"
   | Sat { model } ->
     print_endline "Sat";
     print_s [%sexp (Solver.check_model solver model : unit Or_error.t)]);
  [%expect {|
    Sat
    (Ok ())
    |}]
;;

let%expect_test "proof-producing unsat with ite lowering" =
  let solver =
    Solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let condition = eq (v "c1") (v "c2") in
  assert_ok solver condition;
  assert_ok solver (neq (ite condition (v "x") (v "y")) (v "x"));
  print_proof_result (Solver.solve solver);
  [%expect {| (Unsat (proof_check (Ok ()))) |}]
;;

let%expect_test "ite-lowered proof prints human-readable refutation text" =
  let solver =
    Solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let condition = eq (v "c1") (v "c2") in
  assert_ok solver condition;
  assert_ok solver (neq (ite condition (v "x") (v "y")) (v "x"));
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
      a3: c1 = c2
      a4: (c1 = c2 ∧ x ≠ x) ∨ (c1 ≠ c2 ∧ y ≠ x)
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: c1 = c2   [assumption a3]
      s4: (c1 = c2 ∧ x ≠ x) ∨ (c1 ≠ c2 ∧ y ≠ x)   [assumption a4]
      s5: false   [refutation of [s0, s1, s2, s3, s4]]
        refutation:
          extensions:
            e0 := (c2 = c1 ∧ ¬(x = x))
            e1 := (¬(c2 = c1) ∧ ¬(x = y))
            e2 := (e0 ∨ e1)
          steps:
            r0: c2 = c1   [s3]
            r1: x ≠ x ∨ ¬(e0)   [definition of e0]
            r2: c2 ≠ c1 ∨ ¬(e1)   [definition of e1]
            r3: e0 ∨ e1 ∨ ¬(e2)   [definition of e2]
            r4: e2   [s4]
            r5: x = x   [EUF: x = x via []]
            r6: ⊥   [RUP over [r0, r2, r4, r5, r1, r3]]
    Conclusion: s5

    (check (result (Ok ())))
    |}]
;;
