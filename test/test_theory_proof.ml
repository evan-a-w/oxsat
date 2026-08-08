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
let int64_type : Type_expr.t = Base Int64
let config = { Solver.Config.default with produce_proofs = true }

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

let%expect_test "Int64 integer split proof checks" =
  let solver = Solver.create ~config () in
  Solver.assert_type solver xv int64_type;
  assert_ok
    solver
    (La_compare (La_scale_const (Q.of_int 2, Var xv), `Le, La_const (Q.of_int 3)));
  assert_ok
    solver
    (La_compare (La_scale_const (Q.of_int 2, Var xv), `Ge, La_const (Q.of_int 3)));
  match Solver.solve solver with
  | Sat _ -> print_endline "unexpectedly sat"
  | Unsat { proof; core = _ } ->
    print_s
      [%message
        "proof"
          ~produced:(Option.is_some proof : bool)
          ~checked:
            (Option.value_map proof ~default:false ~f:(fun proof ->
               Proof.check proof |> Or_error.is_ok)
             : bool)];
    [%expect {| (proof (produced true) (checked true)) |}]
;;

let%expect_test "Int64 bound proof checks" =
  let solver = Solver.create ~config () in
  Solver.assert_type solver xv int64_type;
  assert_ok
    solver
    (La_compare (Var xv, `Gt, La_const (Q.of_int64 Int64.max_value)));
  match Solver.solve solver with
  | Sat _ -> print_endline "unexpectedly sat"
  | Unsat { proof; core = _ } ->
    print_s
      [%message
        "proof"
          ~produced:(Option.is_some proof : bool)
          ~checked:
            (Option.value_map proof ~default:false ~f:(fun proof ->
               Proof.check proof |> Or_error.is_ok)
             : bool)];
    [%expect {| (proof (produced true) (checked true)) |}]
;;

let%expect_test "subtype implication proof checks" =
  let solver = Solver.create ~config () in
  Solver.assert_type solver xv int64_type;
  assert_ok solver (Not (Eq (Type_var xv, Int)));
  match Solver.solve solver with
  | Sat _ -> print_endline "unexpectedly sat"
  | Unsat { proof; core = _ } ->
    print_s
      [%message
        "proof"
          ~produced:(Option.is_some proof : bool)
          ~checked:
            (Option.value_map proof ~default:false ~f:(fun proof ->
               Proof.check proof |> Or_error.is_ok)
             : bool)];
    [%expect {| (proof (produced true) (checked true)) |}]
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
      a1: bool ≠ real
      a2: bool ≠ int64
      a3: int ≠ real
      a4: int ≠ int64
      a5: real ≠ int64
      a6: x = y
      a7: y ≠ z
      a8: x = z
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ real   [assumption a1]
      s2: bool ≠ int64   [assumption a2]
      s3: int ≠ real   [assumption a3]
      s4: int ≠ int64   [assumption a4]
      s5: real ≠ int64   [assumption a5]
      s6: x = y   [assumption a6]
      s7: y ≠ z   [assumption a7]
      s8: x = z   [assumption a8]
      s9: false   [refutation of [s0, s1, s2, s3, s4, s5, s6, s7, s8]]
        refutation:
          steps:
            r0: x = y   [s6]
            r1: y ≠ z   [s7]
            r2: x = z   [s8]
            r3: x ≠ y ∨ x ≠ z ∨ y = z   [EUF: y = z via [x = y; x = z]]
            r4: ⊥   [RUP over [r0, r1, r2, r3]]
    Conclusion: s9
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
      a1: bool ≠ real
      a2: bool ≠ int64
      a3: int ≠ real
      a4: int ≠ int64
      a5: real ≠ int64
      a6: x = y
      a7: f(x) ≠ f(y)
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ real   [assumption a1]
      s2: bool ≠ int64   [assumption a2]
      s3: int ≠ real   [assumption a3]
      s4: int ≠ int64   [assumption a4]
      s5: real ≠ int64   [assumption a5]
      s6: x = y   [assumption a6]
      s7: f(x) ≠ f(y)   [assumption a7]
      s8: false   [refutation of [s0, s1, s2, s3, s4, s5, s6, s7]]
        refutation:
          steps:
            r0: x = y   [s6]
            r1: f(x) ≠ f(y)   [s7]
            r2: x ≠ y ∨ f(x) = f(y)   [EUF: f(x) = f(y) via [x = y; congruence(f(x) = f(y) from [x = y])]]
            r3: ⊥   [RUP over [r0, r1, r2]]
    Conclusion: s8
    |}]
;;

let%expect_test "type-theory conflict (Int vs Bool)" =
  let solver = Solver.create ~config () in
  Solver.assert_type solver xv int_type;
  Solver.assert_type solver xv (Base Bool);
  check_proof (Solver.solve solver);
  [%expect
    {|
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ real
      a2: bool ≠ int64
      a3: int ≠ real
      a4: int ≠ int64
      a5: real ≠ int64
      a6: x : int
      a7: x : bool
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ real   [assumption a1]
      s2: bool ≠ int64   [assumption a2]
      s3: int ≠ real   [assumption a3]
      s4: int ≠ int64   [assumption a4]
      s5: real ≠ int64   [assumption a5]
      s6: x : int   [assumption a6]
      s7: x : bool   [assumption a7]
      s8: false   [refutation of [s0, s1, s2, s3, s4, s5, s6, s7]]
        refutation:
          steps:
            r0: x : int   [s6]
            r1: x : bool   [s7]
            r2: ¬(x : bool) ∨ ¬(x : int)   [type clash: bool vs int, given [x : bool, x : int]]
            r3: ⊥   [RUP over [r0, r1, r2]]
    Conclusion: s8
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
      a1: bool ≠ real
      a2: bool ≠ int64
      a3: int ≠ real
      a4: int ≠ int64
      a5: real ≠ int64
      a6: x ≥ 5
      a7: x ≤ 3
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ real   [assumption a1]
      s2: bool ≠ int64   [assumption a2]
      s3: int ≠ real   [assumption a3]
      s4: int ≠ int64   [assumption a4]
      s5: real ≠ int64   [assumption a5]
      s6: x ≥ 5   [assumption a6]
      s7: x ≤ 3   [assumption a7]
      s8: false   [refutation of [s0, s1, s2, s3, s4, s5, s6, s7]]
        refutation:
          steps:
            r0: -x ≤ -5   [s6]
            r1: x ≤ 3   [s7]
            r2: ¬(-x ≤ -5) ∨ ¬(x ≤ 3)   [Farkas: (-x ≤ -5) + (x ≤ 3) ⟹ false]
            r3: ⊥   [RUP over [r0, r1, r2]]
    Conclusion: s8
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
      a1: bool ≠ real
      a2: bool ≠ int64
      a3: int ≠ real
      a4: int ≠ int64
      a5: real ≠ int64
      a6: x : int
      a7: 3*x ≥ 1
      a8: 3*x ≤ 2
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ real   [assumption a1]
      s2: bool ≠ int64   [assumption a2]
      s3: int ≠ real   [assumption a3]
      s4: int ≠ int64   [assumption a4]
      s5: real ≠ int64   [assumption a5]
      s6: x : int   [assumption a6]
      s7: 3*x ≥ 1   [assumption a7]
      s8: 3*x ≤ 2   [assumption a8]
      s9: false   [refutation of [s0, s1, s2, s3, s4, s5, s6, s7, s8]]
        refutation:
          steps:
            r0: x : int   [s6]
            r1: -3x ≤ -1   [s7]
            r2: 3x ≤ 2   [s8]
            r3: ¬(x : int) ∨ -x ≤ -1 ∨ x ≤ 0   [integer split under x : int: x ≤ 0 ∨ x ≥ 1]
            r4: ¬(-x ≤ -1) ∨ ¬(3x ≤ 2)   [Farkas: 3·(-x ≤ -1) + (3x ≤ 2) ⟹ false]
            r5: ¬(-x ≤ -1)   [RUP over [r0, r1, r2, r4]]
            r6: ¬(-3x ≤ -1) ∨ ¬(x ≤ 0)   [Farkas: 3·(x ≤ 0) + (-3x ≤ -1) ⟹ false]
            r7: ⊥   [RUP over [r0, r1, r2, r4, r6, r3]]
    Conclusion: s9
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
      a1: bool ≠ real
      a2: bool ≠ int64
      a3: int ≠ real
      a4: int ≠ int64
      a5: real ≠ int64
      a6: x = y
      a7: x ≥ 3
      a8: x ≤ 3
      a9: y ≤ 2
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ real   [assumption a1]
      s2: bool ≠ int64   [assumption a2]
      s3: int ≠ real   [assumption a3]
      s4: int ≠ int64   [assumption a4]
      s5: real ≠ int64   [assumption a5]
      s6: x = y   [assumption a6]
      s7: x ≥ 3   [assumption a7]
      s8: x ≤ 3   [assumption a8]
      s9: y ≤ 2   [assumption a9]
      s10: false   [refutation of [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9]]
        refutation:
          steps:
            r0: x = y   [s6]
            r1: -x ≤ -3   [s7]
            r2: y ≤ 2   [s9]
            r3: x ≠ y ∨ x + -y ≤ 0   [x = y ⟹ x ≤ y]
            r4: ¬(-x ≤ -3) ∨ ¬(x + -y ≤ 0) ∨ ¬(y ≤ 2)   [Farkas: (x + -y ≤ 0) + (-x ≤ -3) + (y ≤ 2) ⟹ false]
            r5: ⊥   [RUP over [r0, r1, r2, r3, r4]]
    Conclusion: s10
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
      a1: bool ≠ real
      a2: bool ≠ int64
      a3: int ≠ real
      a4: int ≠ int64
      a5: real ≠ int64
      a6: x = y ∨ y = z
      a7: x ≠ y
      a8: y ≠ z
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ real   [assumption a1]
      s2: bool ≠ int64   [assumption a2]
      s3: int ≠ real   [assumption a3]
      s4: int ≠ int64   [assumption a4]
      s5: real ≠ int64   [assumption a5]
      s6: x = y ∨ y = z   [assumption a6]
      s7: x ≠ y   [assumption a7]
      s8: y ≠ z   [assumption a8]
      s9: false   [refutation of [s0, s1, s2, s3, s4, s5, s6, s7, s8]]
        refutation:
          extensions:
            e0 := (x = y ∨ y = z)
          steps:
            r0: x = y ∨ y = z ∨ ¬(e0)   [definition of e0]
            r1: e0   [s6]
            r2: x ≠ y   [s7]
            r3: y ≠ z   [s8]
            r4: ⊥   [RUP over [r1, r2, r3, r0]]
    Conclusion: s9
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
      a1: bool ≠ real
      a2: bool ≠ int64
      a3: int ≠ real
      a4: int ≠ int64
      a5: real ≠ int64
      a6: x = y ∨ x = z
      a7: x ≠ y
      a8: x ≥ 5
      a9: z ≤ 3
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ real   [assumption a1]
      s2: bool ≠ int64   [assumption a2]
      s3: int ≠ real   [assumption a3]
      s4: int ≠ int64   [assumption a4]
      s5: real ≠ int64   [assumption a5]
      s6: x = y ∨ x = z   [assumption a6]
      s7: x ≠ y   [assumption a7]
      s8: x ≥ 5   [assumption a8]
      s9: z ≤ 3   [assumption a9]
      s10: false   [refutation of [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9]]
        refutation:
          extensions:
            e0 := (x = y ∨ x = z)
          steps:
            r0: x = y ∨ x = z ∨ ¬(e0)   [definition of e0]
            r1: e0   [s6]
            r2: x ≠ y   [s7]
            r3: -x ≤ -5   [s8]
            r4: z ≤ 3   [s9]
            r5: x ≠ z ∨ x + -z ≤ 0   [x = z ⟹ x ≤ z]
            r6: ¬(-x ≤ -5) ∨ ¬(x + -z ≤ 0) ∨ ¬(z ≤ 3)   [Farkas: (x + -z ≤ 0) + (-x ≤ -5) + (z ≤ 3) ⟹ false]
            r7: ⊥   [RUP over [r1, r2, r3, r4, r6, r0, r5]]
    Conclusion: s10
    |}]
;;
