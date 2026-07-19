open! Core
open! Feel.Import
open! Theory_core
open! Theory

(* End-to-end proof production: each unsat instance is solved with proofs on and
   the returned proof is re-checked with the solver-independent [Proof.check]. *)

let x : Formula.any = Var (Tvar.of_string "x")
let y : Formula.any = Var (Tvar.of_string "y")
let z : Formula.any = Var (Tvar.of_string "z")
let f arg : Formula.any = App (Tvar.of_string "f", [ arg ])
let eq a b : Formula.any = Eq (a, b)
let neq a b : Formula.any = Not (eq a b)
let xv = Tvar.of_string "x"
let yv = Tvar.of_string "y"
let int_type : Type_expr.t = Base Int
let config = { Solver.Config.produce_proofs = true }

let assert_ok solver formula =
  match Or_error.ok_exn (Solver.assert_formula solver formula) with
  | `Ok -> ()
  | `Unsat _ -> print_endline "UNSAT (at assert time)"
;;

let check_proof (result : Solver_result.t) =
  match result with
  | Sat _ -> print_endline "unexpectedly sat"
  | Unsat { proof; core = _ } ->
    (match proof with
     | None -> print_endline "no proof produced"
     | Some proof -> print_s [%sexp (Proof.check proof : unit Or_error.t)])
;;

let%expect_test "EUF transitivity conflict" =
  let solver = Solver.create ~config () in
  assert_ok solver (eq x y);
  assert_ok solver (neq y z);
  assert_ok solver (eq x z);
  check_proof (Solver.solve solver);
  [%expect {| (Ok ()) |}]
;;

let%expect_test "EUF congruence conflict" =
  let solver = Solver.create ~config () in
  assert_ok solver (eq x y);
  assert_ok solver (neq (f x) (f y));
  check_proof (Solver.solve solver);
  [%expect {| (Ok ()) |}]
;;

let%expect_test "type-theory conflict (Int vs Float)" =
  let solver = Solver.create ~config () in
  Solver.assert_type solver xv int_type;
  Solver.assert_type solver xv (Base Float);
  check_proof (Solver.solve solver);
  [%expect {| (Ok ()) |}]
;;

let%expect_test "linear-arithmetic (Farkas) conflict" =
  let solver = Solver.create ~config () in
  assert_ok solver (Formula.La_compare (x, `Ge, La_const (Q.of_int 5)));
  assert_ok solver (Formula.La_compare (x, `Le, La_const (Q.of_int 3)));
  check_proof (Solver.solve solver);
  [%expect {| (Ok ()) |}]
;;

let%expect_test "integer variable with no feasible integer point" =
  (* 1/3 <= x <= 2/3 has no integer solution, forcing an integer split. *)
  let solver = Solver.create ~config () in
  Solver.assert_type solver xv int_type;
  assert_ok
    solver
    (Formula.La_compare
       (La_scale_const (Q.of_int 3, x), `Ge, La_const (Q.of_int 1)));
  assert_ok
    solver
    (Formula.La_compare
       (La_scale_const (Q.of_int 3, x), `Le, La_const (Q.of_int 2)));
  check_proof (Solver.solve solver);
  [%expect {| (Ok ()) |}]
;;

let%expect_test "Nelson-Oppen bridge (bare-var-eq + LA)" =
  let solver = Solver.create ~config () in
  assert_ok solver (eq x y);
  assert_ok solver (Formula.La_compare (x, `Ge, La_const (Q.of_int 3)));
  assert_ok solver (Formula.La_compare (x, `Le, La_const (Q.of_int 3)));
  assert_ok solver (Formula.La_compare (y, `Le, La_const (Q.of_int 2)));
  check_proof (Solver.solve solver);
  [%expect {| (Ok ()) |}]
;;

let%expect_test "propositional-over-atoms conflict" =
  let solver = Solver.create ~config () in
  let a = eq x y in
  let b = eq y z in
  assert_ok solver (Or [ a; b ]);
  assert_ok solver (Not a);
  assert_ok solver (Not b);
  check_proof (Solver.solve solver);
  [%expect {| (Ok ()) |}]
;;
