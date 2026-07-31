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
  let solver = Solver.create ~config:{ produce_proofs = true } () in
  let condition = eq (v "c1") (v "c2") in
  assert_ok solver condition;
  assert_ok solver (neq (ite condition (v "x") (v "y")) (v "x"));
  print_proof_result (Solver.solve solver);
  [%expect {| (Unsat (proof_check (Ok ()))) |}]
;;
