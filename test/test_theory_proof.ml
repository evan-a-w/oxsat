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
     | Some proof ->
       assert (Proof.check proof |> Or_error.is_ok);
       print_string (Proof.to_string_hum proof))
;;

let%expect_test "EUF transitivity conflict" =
  let solver = Solver.create ~config () in
  assert_ok solver (eq x y);
  assert_ok solver (neq y z);
  assert_ok solver (eq x z);
  check_proof (Solver.solve solver);
  [%expect
    {|
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: x = y
      a4: y ≠ z
      a5: x = z
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: x = y   [assumption a3]
      s4: y ≠ z   [assumption a4]
      s5: x = z   [assumption a5]
      s6: false   [refutation of [s0, s1, s2, s3, s4, s5]]
        refutation:
          steps:
            r0: x = y   [assumption a3]
            r1: y ≠ z   [assumption a4]
            r2: x = z   [assumption a5]
            r3: x ≠ y ∨ x ≠ z ∨ y = z   [EUF: y = z via [x = y; x = z]]
            r4: ⊥   [RUP over [r0, r1, r2, r3]]
    Conclusion: s6
    |}]
;;

let%expect_test "EUF congruence conflict" =
  let solver = Solver.create ~config () in
  assert_ok solver (eq x y);
  assert_ok solver (neq (f x) (f y));
  check_proof (Solver.solve solver);
  [%expect
    {|
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: x = y
      a4: f(x) ≠ f(y)
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: x = y   [assumption a3]
      s4: f(x) ≠ f(y)   [assumption a4]
      s5: false   [refutation of [s0, s1, s2, s3, s4]]
        refutation:
          steps:
            r0: x = y   [assumption a3]
            r1: f(x) ≠ f(y)   [assumption a4]
            r2: x ≠ y ∨ f(x) = f(y)   [EUF: f(x) = f(y) via [x = y; congruence(f(x) = f(y) from [x = y])]]
            r3: ⊥   [RUP over [r0, r1, r2]]
    Conclusion: s5
    |}]
;;

let%expect_test "type-theory conflict (Int vs Float)" =
  let solver = Solver.create ~config () in
  Solver.assert_type solver xv int_type;
  Solver.assert_type solver xv (Base Float);
  check_proof (Solver.solve solver);
  [%expect
    {|
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: x : int
      a4: x : float
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: x : int   [assumption a3]
      s4: x : float   [assumption a4]
      s5: false   [refutation of [s0, s1, s2, s3, s4]]
        refutation:
          steps:
            r0: int ≠ float   [assumption a2]
            r1: x : int   [assumption a3]
            r2: x : float   [assumption a4]
            r3: ¬(x : int) ∨ ¬(x : float) ∨ int = float   [EUF: int = float via [x : int; x : float]]
            r4: ⊥   [RUP over [r0, r1, r2, r3]]
    Conclusion: s5
    |}]
;;

let%expect_test "linear-arithmetic (Farkas) conflict" =
  let solver = Solver.create ~config () in
  assert_ok solver (Formula.La_compare (x, `Ge, La_const (Q.of_int 5)));
  assert_ok solver (Formula.La_compare (x, `Le, La_const (Q.of_int 3)));
  check_proof (Solver.solve solver);
  [%expect
    {|
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: x ≥ 5
      a4: x ≤ 3
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: x ≥ 5   [assumption a3]
      s4: x ≤ 3   [assumption a4]
      s5: false   [refutation of [s0, s1, s2, s3, s4]]
        refutation:
          steps:
            r0: -x ≤ -5   [assumption a3]
            r1: x ≤ 3   [assumption a4]
            r2: ¬(-x ≤ -5) ∨ ¬(x ≤ 3)   [Farkas: (-x ≤ -5) + (x ≤ 3) ⟹ false]
            r3: ⊥   [RUP over [r0, r1, r2]]
    Conclusion: s5
    |}]
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
  [%expect
    {|
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: x : int
      a4: 3*x ≥ 1
      a5: 3*x ≤ 2
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: x : int   [assumption a3]
      s4: 3*x ≥ 1   [assumption a4]
      s5: 3*x ≤ 2   [assumption a5]
      s6: false   [refutation of [s0, s1, s2, s3, s4, s5]]
        refutation:
          steps:
            r0: x : int   [assumption a3]
            r1: -3x ≤ -1   [assumption a4]
            r2: 3x ≤ 2   [assumption a5]
            r3: ¬(x : int) ∨ -x ≤ -1 ∨ x ≤ 0   [integer split: x ≤ 0 ∨ x ≥ 1]
            r4: ¬(-x ≤ -1) ∨ ¬(3x ≤ 2)   [Farkas: 3·(-x ≤ -1) + (3x ≤ 2) ⟹ false]
            r5: ¬(-x ≤ -1)   [RUP over [r0, r1, r2, r4]]
            r6: ¬(-3x ≤ -1) ∨ ¬(x ≤ 0)   [Farkas: 3·(x ≤ 0) + (-3x ≤ -1) ⟹ false]
            r7: ⊥   [RUP over [r0, r1, r2, r4, r6, r3]]
    Conclusion: s6
    |}]
;;

let%expect_test "Nelson-Oppen bridge (bare-var-eq + LA)" =
  let solver = Solver.create ~config () in
  assert_ok solver (eq x y);
  assert_ok solver (Formula.La_compare (x, `Ge, La_const (Q.of_int 3)));
  assert_ok solver (Formula.La_compare (x, `Le, La_const (Q.of_int 3)));
  assert_ok solver (Formula.La_compare (y, `Le, La_const (Q.of_int 2)));
  check_proof (Solver.solve solver);
  [%expect
    {|
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: x = y
      a4: x ≥ 3
      a5: x ≤ 3
      a6: y ≤ 2
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: x = y   [assumption a3]
      s4: x ≥ 3   [assumption a4]
      s5: x ≤ 3   [assumption a5]
      s6: y ≤ 2   [assumption a6]
      s7: false   [refutation of [s0, s1, s2, s3, s4, s5, s6]]
        refutation:
          steps:
            r0: x = y   [assumption a3]
            r1: -x ≤ -3   [assumption a4]
            r2: y ≤ 2   [assumption a6]
            r3: x ≠ y ∨ x + -y ≤ 0   [x = y ⟹ x ≤ y]
            r4: ¬(-x ≤ -3) ∨ ¬(x + -y ≤ 0) ∨ ¬(y ≤ 2)   [Farkas: (x + -y ≤ 0) + (-x ≤ -3) + (y ≤ 2) ⟹ false]
            r5: ⊥   [RUP over [r0, r1, r2, r3, r4]]
    Conclusion: s7
    |}]
;;

let%expect_test "propositional-over-atoms conflict" =
  let solver = Solver.create ~config () in
  let a = eq x y in
  let b = eq y z in
  assert_ok solver (Or [ a; b ]);
  assert_ok solver (Not a);
  assert_ok solver (Not b);
  check_proof (Solver.solve solver);
  [%expect
    {|
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: x = y ∨ y = z
      a4: x ≠ y
      a5: y ≠ z
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: x = y ∨ y = z   [assumption a3]
      s4: x ≠ y   [assumption a4]
      s5: y ≠ z   [assumption a5]
      s6: false   [refutation of [s0, s1, s2, s3, s4, s5]]
        refutation:
          extensions:
            e0 := (x = y ∨ y = z)
          steps:
            r0: x = y ∨ y = z ∨ ¬(e0)   [definition of e0]
            r1: e0   [assumption a3]
            r2: x ≠ y   [assumption a4]
            r3: y ≠ z   [assumption a5]
            r4: ⊥   [RUP over [r1, r2, r3, r0]]
    Conclusion: s6
    |}]
;;

(* A worked, human-followable multi-step refutation combining a boolean
   case-split, Nelson-Oppen theory combination, and a Farkas argument.

   Assertions: [x = y \/ x = z], [x <> y], [x >= 5], [z <= 3].

   The disjunction can only hold via [x = z] (since [x <> y]); but [x = z]
   forces [x - z <= 0], which with [x >= 5] and [z <= 3] is arithmetically
   impossible. Reading the refutation's [steps] top to bottom:

   - step 0: the Tseitin definition of the [Or] (extension variable 0).
   - steps 1-2: the disjunction holds (ext 0 true) and [x <> y].
     Unit-propagation over these forces the [x = z] disjunct.
   - steps 3-4: the input bounds [x >= 5] and [z <= 3], as [`Le] atoms.
   - step 5: a [Bare_var_eq] lemma -- [x = z] implies [x - z <= 0].
   - step 6: a [Linear_arithmetic] (Farkas) lemma -- the non-negative
     combination [1*(x>=5) + 1*(x-z<=0) + 1*(z<=3)] yields [5 <= 3], false.
   - step 7: the empty clause by reverse unit propagation over the above. *)
let%expect_test "case-split with Nelson-Oppen + Farkas reasoning" =
  let solver = Solver.create ~config () in
  assert_ok solver (Or [ eq x y; eq x z ]);
  assert_ok solver (neq x y);
  assert_ok solver (Formula.La_compare (x, `Ge, La_const (Q.of_int 5)));
  assert_ok solver (Formula.La_compare (z, `Le, La_const (Q.of_int 3)));
  check_proof (Solver.solve solver);
  [%expect
    {|
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: x = y ∨ x = z
      a4: x ≠ y
      a5: x ≥ 5
      a6: z ≤ 3
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: x = y ∨ x = z   [assumption a3]
      s4: x ≠ y   [assumption a4]
      s5: x ≥ 5   [assumption a5]
      s6: z ≤ 3   [assumption a6]
      s7: false   [refutation of [s0, s1, s2, s3, s4, s5, s6]]
        refutation:
          extensions:
            e0 := (x = y ∨ x = z)
          steps:
            r0: x = y ∨ x = z ∨ ¬(e0)   [definition of e0]
            r1: e0   [assumption a3]
            r2: x ≠ y   [assumption a4]
            r3: -x ≤ -5   [assumption a5]
            r4: z ≤ 3   [assumption a6]
            r5: x ≠ z ∨ x + -z ≤ 0   [x = z ⟹ x ≤ z]
            r6: ¬(-x ≤ -5) ∨ ¬(x + -z ≤ 0) ∨ ¬(z ≤ 3)   [Farkas: (x + -z ≤ 0) + (-x ≤ -5) + (z ≤ 3) ⟹ false]
            r7: ⊥   [RUP over [r1, r2, r3, r4, r6, r0, r5]]
    Conclusion: s7
    |}]
;;
