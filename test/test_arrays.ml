open! Core
open! Feel.Import
open! Theory_core
open! Theory

let v name : Formula.any = Var (Tvar.of_string name)
let a = v "a"
let b = v "b"
let i = v "i"
let j = v "j"
let value = v "value"
let other = v "other"
let select array index : Formula.any = Select (array, index)
let store array index value : Formula.any = Store (array, index, value)
let eq left right : Formula.any = Eq (left, right)
let neq left right : Formula.any = Not (eq left right)

let assert_ok solver formula =
  match Or_error.ok_exn (Solver.assert_formula solver formula) with
  | `Ok -> ()
  | `Unsat _ -> print_endline "Unsat at assert time"
;;

let print_solver_result = function
  | Solver_result.Unsat _ -> print_endline "Unsat"
  | Sat _ -> print_endline "Sat"
;;

let print_proof_result = function
  | Solver_result.Sat _ -> print_endline "Sat"
  | Unsat { proof = Some proof; _ } ->
    print_s
      [%message "Unsat" ~proof_check:(Proof.check proof : unit Or_error.t)]
  | Unsat { proof = None; _ } -> print_endline "Unsat without proof"
;;

let%expect_test "read over write at the same index" =
  let solver = Solver.create () in
  assert_ok solver (neq (select (store a i value) i) value);
  print_solver_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "array lemmas have checkable proof certificates" =
  let solver = Solver.create ~config:{ produce_proofs = true } () in
  assert_ok solver (neq (select (store a i value) i) value);
  print_proof_result (Solver.solve solver);
  [%expect {| (Unsat (proof_check (Ok ()))) |}]
;;

let%expect_test "read over write at a different index" =
  let solver = Solver.create () in
  assert_ok solver (neq i j);
  assert_ok solver (neq (select (store a i value) j) (select a j));
  print_solver_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "array extensionality with a universal select equality" =
  let solver = Quantifier_solver.create () in
  let k = Tvar.of_string "k" in
  ignore
    (Quantifier_solver.assert_formula
       solver
       (Forall
          ( [ k ]
          , [ [ select a (Var k) ]; [ select b (Var k) ] ]
          , eq (select a (Var k)) (select b (Var k)) ))
     : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       solver
       (Formula.widen_quantified (eq (select a i) (select a i)))
     : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       solver
       (Formula.widen_quantified (eq (select b i) (select b i)))
     : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       solver
       (Formula.widen_quantified (neq a b))
     : _ Or_error.t);
  (match Quantifier_solver.solve solver ~max_rounds:4 with
   | Unsat _ -> print_endline "Unsat"
   | Sat _ -> print_endline "Sat"
   | Unknown_but_possibly_sat _ -> print_endline "Unknown");
  [%expect {| Unsat |}]
;;

let%expect_test "satisfiable array problem has a checkable model" =
  let solver = Solver.create () in
  assert_ok solver (neq i j);
  assert_ok solver (eq (select (store a i value) j) other);
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

let%expect_test "array indices interact with linear arithmetic" =
  let solver = Solver.create () in
  assert_ok solver (La_compare (i, `Lt, j));
  assert_ok solver (neq (select (store a i value) j) (select a j));
  print_solver_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "array indices interact with linear arithmetic with proofs" =
  let solver = Solver.create ~config:{ produce_proofs = true } () in
  assert_ok solver (La_compare (i, `Lt, j));
  assert_ok solver (neq (select (store a i value) j) (select a j));
  print_proof_result (Solver.solve solver);
  [%expect {| (Unsat (proof_check (Ok ()))) |}]
;;

let%expect_test "distinct arrays with a differing read stay satisfiable" =
  let solver = Solver.create () in
  assert_ok solver (neq a b);
  assert_ok solver (neq (select a i) (select b i));
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

let%expect_test "extensionality through aliased array variables" =
  let solver = Quantifier_solver.create () in
  let x = v "x" in
  let y = v "y" in
  let k = Tvar.of_string "k" in
  let assert_q f =
    ignore (Quantifier_solver.assert_formula solver f : _ Or_error.t)
  in
  assert_q
    (Forall
       ( [ k ]
       , [ [ select x (Var k) ]; [ select y (Var k) ] ]
       , eq (select x (Var k)) (select y (Var k)) ));
  assert_q (Formula.widen_quantified (eq (select x i) (select x i)));
  assert_q (Formula.widen_quantified (eq (select y i) (select y i)));
  assert_q (Formula.widen_quantified (eq x a));
  assert_q (Formula.widen_quantified (eq y b));
  assert_q (Formula.widen_quantified (neq a b));
  (match Quantifier_solver.solve solver ~max_rounds:6 with
   | Unsat _ -> print_endline "Unsat"
   | Sat _ -> print_endline "Sat"
   | Unknown_but_possibly_sat _ -> print_endline "Unknown");
  [%expect {| Unsat |}]
;;
