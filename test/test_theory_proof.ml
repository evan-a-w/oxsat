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
       print_s [%sexp (proof : Proof.t)])
;;

let%expect_test "EUF transitivity conflict" =
  let solver = Solver.create ~config () in
  assert_ok solver (eq x y);
  assert_ok solver (neq y z);
  assert_ok solver (eq x z);
  check_proof (Solver.solve solver);
  [%expect {|
    ((assumptions
      (((name ()) (formula (Not (Eq Bool Int))))
       ((name ()) (formula (Not (Eq Bool Float))))
       ((name ()) (formula (Not (Eq Int Float))))
       ((name ()) (formula (Eq (Var x) (Var y))))
       ((name ()) (formula (Not (Eq (Var y) (Var z)))))
       ((name ()) (formula (Eq (Var x) (Var z))))))
     (steps
      (((name ()) (conclusion (Not (Eq Bool Int)))
        (justification (Assumption 0)))
       ((name ()) (conclusion (Not (Eq Bool Float)))
        (justification (Assumption 1)))
       ((name ()) (conclusion (Not (Eq Int Float)))
        (justification (Assumption 2)))
       ((name ()) (conclusion (Eq (Var x) (Var y)))
        (justification (Assumption 3)))
       ((name ()) (conclusion (Not (Eq (Var y) (Var z))))
        (justification (Assumption 4)))
       ((name ()) (conclusion (Eq (Var x) (Var z)))
        (justification (Assumption 5)))
       ((name ()) (conclusion False)
        (justification
         (By_refutation (premises (0 1 2 3 4 5))
          (refutation
           ((inputs
             ((Not (Eq Bool Int)) (Not (Eq Bool Float)) (Not (Eq Int Float))
              (Eq (Var x) (Var y)) (Not (Eq (Var y) (Var z)))
              (Eq (Var x) (Var z)) (Not False)))
            (extensions ())
            (steps
             (((clause
                (((atom (Theory (Eq ((Var x) (Var y))))) (positive true))))
               (reason
                (Input_clause
                 ((input 3)
                  (literal
                   ((atom (Theory (Eq ((Var x) (Var y))))) (positive true)))))))
              ((clause
                (((atom (Theory (Eq ((Var y) (Var z))))) (positive false))))
               (reason
                (Input_clause
                 ((input 4)
                  (literal
                   ((atom (Theory (Eq ((Var y) (Var z))))) (positive false)))))))
              ((clause
                (((atom (Theory (Eq ((Var x) (Var z))))) (positive true))))
               (reason
                (Input_clause
                 ((input 5)
                  (literal
                   ((atom (Theory (Eq ((Var x) (Var z))))) (positive true)))))))
              ((clause
                (((atom (Theory (Eq ((Var x) (Var y))))) (positive false))
                 ((atom (Theory (Eq ((Var x) (Var z))))) (positive false))
                 ((atom (Theory (Eq ((Var y) (Var z))))) (positive true))))
               (reason
                (Theory_lemma
                 (Euf
                  (Equality
                   ((conclusion ((left (Var y)) (right (Var z))))
                    (path
                     ((Asserted (clause_literal 0))
                      (Asserted (clause_literal 1))))))))))
              ((clause ()) (reason (Rup (hints (0 1 2 3)))))))
            (contradiction 4))))))))
     (conclusion 6))
    |}]
;;

let%expect_test "EUF congruence conflict" =
  let solver = Solver.create ~config () in
  assert_ok solver (eq x y);
  assert_ok solver (neq (f x) (f y));
  check_proof (Solver.solve solver);
  [%expect {|
    ((assumptions
      (((name ()) (formula (Not (Eq Bool Int))))
       ((name ()) (formula (Not (Eq Bool Float))))
       ((name ()) (formula (Not (Eq Int Float))))
       ((name ()) (formula (Eq (Var x) (Var y))))
       ((name ()) (formula (Not (Eq (App f ((Var x))) (App f ((Var y)))))))))
     (steps
      (((name ()) (conclusion (Not (Eq Bool Int)))
        (justification (Assumption 0)))
       ((name ()) (conclusion (Not (Eq Bool Float)))
        (justification (Assumption 1)))
       ((name ()) (conclusion (Not (Eq Int Float)))
        (justification (Assumption 2)))
       ((name ()) (conclusion (Eq (Var x) (Var y)))
        (justification (Assumption 3)))
       ((name ()) (conclusion (Not (Eq (App f ((Var x))) (App f ((Var y))))))
        (justification (Assumption 4)))
       ((name ()) (conclusion False)
        (justification
         (By_refutation (premises (0 1 2 3 4))
          (refutation
           ((inputs
             ((Not (Eq Bool Int)) (Not (Eq Bool Float)) (Not (Eq Int Float))
              (Eq (Var x) (Var y)) (Not (Eq (App f ((Var x))) (App f ((Var y)))))
              (Not False)))
            (extensions ())
            (steps
             (((clause
                (((atom (Theory (Eq ((Var x) (Var y))))) (positive true))))
               (reason
                (Input_clause
                 ((input 3)
                  (literal
                   ((atom (Theory (Eq ((Var x) (Var y))))) (positive true)))))))
              ((clause
                (((atom (Theory (Eq ((App f ((Var x))) (App f ((Var y)))))))
                  (positive false))))
               (reason
                (Input_clause
                 ((input 4)
                  (literal
                   ((atom (Theory (Eq ((App f ((Var x))) (App f ((Var y)))))))
                    (positive false)))))))
              ((clause
                (((atom (Theory (Eq ((Var x) (Var y))))) (positive false))
                 ((atom (Theory (Eq ((App f ((Var x))) (App f ((Var y)))))))
                  (positive true))))
               (reason
                (Theory_lemma
                 (Euf
                  (Equality
                   ((conclusion
                     ((left (App f ((Var x)))) (right (App f ((Var y))))))
                    (path
                     ((Asserted (clause_literal 0))
                      (Congruence (left (App f ((Var x))))
                       (right (App f ((Var y))))
                       (argument_equalities (((left (Var x)) (right (Var y))))))))))))))
              ((clause ()) (reason (Rup (hints (0 1 2)))))))
            (contradiction 3))))))))
     (conclusion 5))
    |}]
;;

let%expect_test "type-theory conflict (Int vs Float)" =
  let solver = Solver.create ~config () in
  Solver.assert_type solver xv int_type;
  Solver.assert_type solver xv (Base Float);
  check_proof (Solver.solve solver);
  [%expect {|
    ((assumptions
      (((name ()) (formula (Not (Eq Bool Int))))
       ((name ()) (formula (Not (Eq Bool Float))))
       ((name ()) (formula (Not (Eq Int Float))))
       ((name ()) (formula (Eq (Type_var x) Int)))
       ((name ()) (formula (Eq (Type_var x) Float)))))
     (steps
      (((name ()) (conclusion (Not (Eq Bool Int)))
        (justification (Assumption 0)))
       ((name ()) (conclusion (Not (Eq Bool Float)))
        (justification (Assumption 1)))
       ((name ()) (conclusion (Not (Eq Int Float)))
        (justification (Assumption 2)))
       ((name ()) (conclusion (Eq (Type_var x) Int))
        (justification (Assumption 3)))
       ((name ()) (conclusion (Eq (Type_var x) Float))
        (justification (Assumption 4)))
       ((name ()) (conclusion False)
        (justification
         (By_refutation (premises (0 1 2 3 4))
          (refutation
           ((inputs
             ((Not (Eq Bool Int)) (Not (Eq Bool Float)) (Not (Eq Int Float))
              (Eq (Type_var x) Int) (Eq (Type_var x) Float) (Not False)))
            (extensions ())
            (steps
             (((clause
                (((atom (Theory (Type_eq ((Base Int) (Base Float)))))
                  (positive false))))
               (reason
                (Input_clause
                 ((input 2)
                  (literal
                   ((atom (Theory (Type_eq ((Base Int) (Base Float)))))
                    (positive false)))))))
              ((clause
                (((atom (Theory (Type_eq ((Var x) (Base Int))))) (positive true))))
               (reason
                (Input_clause
                 ((input 3)
                  (literal
                   ((atom (Theory (Type_eq ((Var x) (Base Int)))))
                    (positive true)))))))
              ((clause
                (((atom (Theory (Type_eq ((Var x) (Base Float)))))
                  (positive true))))
               (reason
                (Input_clause
                 ((input 4)
                  (literal
                   ((atom (Theory (Type_eq ((Var x) (Base Float)))))
                    (positive true)))))))
              ((clause
                (((atom (Theory (Type_eq ((Var x) (Base Int)))))
                  (positive false))
                 ((atom (Theory (Type_eq ((Var x) (Base Float)))))
                  (positive false))
                 ((atom (Theory (Type_eq ((Base Int) (Base Float)))))
                  (positive true))))
               (reason
                (Theory_lemma
                 (Euf
                  (Equality
                   ((conclusion ((left Int) (right Float)))
                    (path
                     ((Asserted (clause_literal 0))
                      (Asserted (clause_literal 1))))))))))
              ((clause ()) (reason (Rup (hints (0 1 2 3)))))))
            (contradiction 4))))))))
     (conclusion 5))
    |}]
;;

let%expect_test "linear-arithmetic (Farkas) conflict" =
  let solver = Solver.create ~config () in
  assert_ok solver (Formula.La_compare (x, `Ge, La_const (Q.of_int 5)));
  assert_ok solver (Formula.La_compare (x, `Le, La_const (Q.of_int 3)));
  check_proof (Solver.solve solver);
  [%expect {|
    ((assumptions
      (((name ()) (formula (Not (Eq Bool Int))))
       ((name ()) (formula (Not (Eq Bool Float))))
       ((name ()) (formula (Not (Eq Int Float))))
       ((name ()) (formula (La_compare (Var x) Ge (La_const ((num 5) (den 1))))))
       ((name ()) (formula (La_compare (Var x) Le (La_const ((num 3) (den 1))))))))
     (steps
      (((name ()) (conclusion (Not (Eq Bool Int)))
        (justification (Assumption 0)))
       ((name ()) (conclusion (Not (Eq Bool Float)))
        (justification (Assumption 1)))
       ((name ()) (conclusion (Not (Eq Int Float)))
        (justification (Assumption 2)))
       ((name ())
        (conclusion (La_compare (Var x) Ge (La_const ((num 5) (den 1)))))
        (justification (Assumption 3)))
       ((name ())
        (conclusion (La_compare (Var x) Le (La_const ((num 3) (den 1)))))
        (justification (Assumption 4)))
       ((name ()) (conclusion False)
        (justification
         (By_refutation (premises (0 1 2 3 4))
          (refutation
           ((inputs
             ((Not (Eq Bool Int)) (Not (Eq Bool Float)) (Not (Eq Int Float))
              (La_compare (Var x) Ge (La_const ((num 5) (den 1))))
              (La_compare (Var x) Le (La_const ((num 3) (den 1)))) (Not False)))
            (extensions ())
            (steps
             (((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num -5) (den 1))))))
                  (positive true))))
               (reason
                (Input_clause
                 ((input 3)
                  (literal
                   ((atom
                     (Theory
                      (Le
                       (((coeffs ((x ((num -1) (den 1)))))
                         (const ((num 0) (den 1))))
                        ((num -5) (den 1))))))
                    (positive true)))))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num 1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 3) (den 1))))))
                  (positive true))))
               (reason
                (Input_clause
                 ((input 4)
                  (literal
                   ((atom
                     (Theory
                      (Le
                       (((coeffs ((x ((num 1) (den 1)))))
                         (const ((num 0) (den 1))))
                        ((num 3) (den 1))))))
                    (positive true)))))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num -5) (den 1))))))
                  (positive false))
                 ((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num 1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 3) (den 1))))))
                  (positive false))))
               (reason
                (Theory_lemma
                 (Linear_arithmetic
                  ((combination
                    (((clause_literal 0) (coefficient ((num 1) (den 1))))
                     ((clause_literal 1) (coefficient ((num 1) (den 1)))))))))))
              ((clause ()) (reason (Rup (hints (0 1 2)))))))
            (contradiction 3))))))))
     (conclusion 5))
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
  [%expect {|
    ((assumptions
      (((name ()) (formula (Not (Eq Bool Int))))
       ((name ()) (formula (Not (Eq Bool Float))))
       ((name ()) (formula (Not (Eq Int Float))))
       ((name ()) (formula (Eq (Type_var x) Int)))
       ((name ())
        (formula
         (La_compare (La_scale_const ((num 3) (den 1)) (Var x)) Ge
          (La_const ((num 1) (den 1))))))
       ((name ())
        (formula
         (La_compare (La_scale_const ((num 3) (den 1)) (Var x)) Le
          (La_const ((num 2) (den 1))))))))
     (steps
      (((name ()) (conclusion (Not (Eq Bool Int)))
        (justification (Assumption 0)))
       ((name ()) (conclusion (Not (Eq Bool Float)))
        (justification (Assumption 1)))
       ((name ()) (conclusion (Not (Eq Int Float)))
        (justification (Assumption 2)))
       ((name ()) (conclusion (Eq (Type_var x) Int))
        (justification (Assumption 3)))
       ((name ())
        (conclusion
         (La_compare (La_scale_const ((num 3) (den 1)) (Var x)) Ge
          (La_const ((num 1) (den 1)))))
        (justification (Assumption 4)))
       ((name ())
        (conclusion
         (La_compare (La_scale_const ((num 3) (den 1)) (Var x)) Le
          (La_const ((num 2) (den 1)))))
        (justification (Assumption 5)))
       ((name ()) (conclusion False)
        (justification
         (By_refutation (premises (0 1 2 3 4 5))
          (refutation
           ((inputs
             ((Not (Eq Bool Int)) (Not (Eq Bool Float)) (Not (Eq Int Float))
              (Eq (Type_var x) Int)
              (La_compare (La_scale_const ((num 3) (den 1)) (Var x)) Ge
               (La_const ((num 1) (den 1))))
              (La_compare (La_scale_const ((num 3) (den 1)) (Var x)) Le
               (La_const ((num 2) (den 1))))
              (Not False)))
            (extensions ())
            (steps
             (((clause
                (((atom (Theory (Type_eq ((Var x) (Base Int))))) (positive true))))
               (reason
                (Input_clause
                 ((input 3)
                  (literal
                   ((atom (Theory (Type_eq ((Var x) (Base Int)))))
                    (positive true)))))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num -3) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num -1) (den 1))))))
                  (positive true))))
               (reason
                (Input_clause
                 ((input 4)
                  (literal
                   ((atom
                     (Theory
                      (Le
                       (((coeffs ((x ((num -3) (den 1)))))
                         (const ((num 0) (den 1))))
                        ((num -1) (den 1))))))
                    (positive true)))))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num 3) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 2) (den 1))))))
                  (positive true))))
               (reason
                (Input_clause
                 ((input 5)
                  (literal
                   ((atom
                     (Theory
                      (Le
                       (((coeffs ((x ((num 3) (den 1)))))
                         (const ((num 0) (den 1))))
                        ((num 2) (den 1))))))
                    (positive true)))))))
              ((clause
                (((atom (Theory (Type_eq ((Var x) (Base Int)))))
                  (positive false))
                 ((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num -1) (den 1))))))
                  (positive true))
                 ((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num 1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 0) (den 1))))))
                  (positive true))))
               (reason
                (Theory_lemma
                 (Integer_split
                  ((variable x) (floor ((num 0) (den 1)))
                   (ceil ((num 1) (den 1))))))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num -1) (den 1))))))
                  (positive false))
                 ((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num 3) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 2) (den 1))))))
                  (positive false))))
               (reason
                (Theory_lemma
                 (Linear_arithmetic
                  ((combination
                    (((clause_literal 0) (coefficient ((num 3) (den 1))))
                     ((clause_literal 1) (coefficient ((num 1) (den 1)))))))))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num -1) (den 1))))))
                  (positive false))))
               (reason (Rup (hints (0 1 2 4)))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num -3) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num -1) (den 1))))))
                  (positive false))
                 ((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num 1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 0) (den 1))))))
                  (positive false))))
               (reason
                (Theory_lemma
                 (Linear_arithmetic
                  ((combination
                    (((clause_literal 1) (coefficient ((num 3) (den 1))))
                     ((clause_literal 0) (coefficient ((num 1) (den 1)))))))))))
              ((clause ()) (reason (Rup (hints (0 1 2 4 6 3)))))))
            (contradiction 7))))))))
     (conclusion 6))
    |}]
;;

let%expect_test "Nelson-Oppen bridge (bare-var-eq + LA)" =
  let solver = Solver.create ~config () in
  assert_ok solver (eq x y);
  assert_ok solver (Formula.La_compare (x, `Ge, La_const (Q.of_int 3)));
  assert_ok solver (Formula.La_compare (x, `Le, La_const (Q.of_int 3)));
  assert_ok solver (Formula.La_compare (y, `Le, La_const (Q.of_int 2)));
  check_proof (Solver.solve solver);
  [%expect {|
    ((assumptions
      (((name ()) (formula (Not (Eq Bool Int))))
       ((name ()) (formula (Not (Eq Bool Float))))
       ((name ()) (formula (Not (Eq Int Float))))
       ((name ()) (formula (Eq (Var x) (Var y))))
       ((name ()) (formula (La_compare (Var x) Ge (La_const ((num 3) (den 1))))))
       ((name ()) (formula (La_compare (Var x) Le (La_const ((num 3) (den 1))))))
       ((name ()) (formula (La_compare (Var y) Le (La_const ((num 2) (den 1))))))))
     (steps
      (((name ()) (conclusion (Not (Eq Bool Int)))
        (justification (Assumption 0)))
       ((name ()) (conclusion (Not (Eq Bool Float)))
        (justification (Assumption 1)))
       ((name ()) (conclusion (Not (Eq Int Float)))
        (justification (Assumption 2)))
       ((name ()) (conclusion (Eq (Var x) (Var y)))
        (justification (Assumption 3)))
       ((name ())
        (conclusion (La_compare (Var x) Ge (La_const ((num 3) (den 1)))))
        (justification (Assumption 4)))
       ((name ())
        (conclusion (La_compare (Var x) Le (La_const ((num 3) (den 1)))))
        (justification (Assumption 5)))
       ((name ())
        (conclusion (La_compare (Var y) Le (La_const ((num 2) (den 1)))))
        (justification (Assumption 6)))
       ((name ()) (conclusion False)
        (justification
         (By_refutation (premises (0 1 2 3 4 5 6))
          (refutation
           ((inputs
             ((Not (Eq Bool Int)) (Not (Eq Bool Float)) (Not (Eq Int Float))
              (Eq (Var x) (Var y))
              (La_compare (Var x) Ge (La_const ((num 3) (den 1))))
              (La_compare (Var x) Le (La_const ((num 3) (den 1))))
              (La_compare (Var y) Le (La_const ((num 2) (den 1)))) (Not False)))
            (extensions ())
            (steps
             (((clause
                (((atom (Theory (Eq ((Var x) (Var y))))) (positive true))))
               (reason
                (Input_clause
                 ((input 3)
                  (literal
                   ((atom (Theory (Eq ((Var x) (Var y))))) (positive true)))))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num -3) (den 1))))))
                  (positive true))))
               (reason
                (Input_clause
                 ((input 4)
                  (literal
                   ((atom
                     (Theory
                      (Le
                       (((coeffs ((x ((num -1) (den 1)))))
                         (const ((num 0) (den 1))))
                        ((num -3) (den 1))))))
                    (positive true)))))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((y ((num 1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 2) (den 1))))))
                  (positive true))))
               (reason
                (Input_clause
                 ((input 6)
                  (literal
                   ((atom
                     (Theory
                      (Le
                       (((coeffs ((y ((num 1) (den 1)))))
                         (const ((num 0) (den 1))))
                        ((num 2) (den 1))))))
                    (positive true)))))))
              ((clause
                (((atom (Theory (Eq ((Var x) (Var y))))) (positive false))
                 ((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num 1) (den 1))) (y ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 0) (den 1))))))
                  (positive true))))
               (reason
                (Theory_lemma
                 (Bare_var_eq
                  (Equality_implies_le (left x) (right y)
                   (direction Left_le_right))))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num -3) (den 1))))))
                  (positive false))
                 ((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num 1) (den 1))) (y ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 0) (den 1))))))
                  (positive false))
                 ((atom
                   (Theory
                    (Le
                     (((coeffs ((y ((num 1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 2) (den 1))))))
                  (positive false))))
               (reason
                (Theory_lemma
                 (Linear_arithmetic
                  ((combination
                    (((clause_literal 1) (coefficient ((num 1) (den 1))))
                     ((clause_literal 0) (coefficient ((num 1) (den 1))))
                     ((clause_literal 2) (coefficient ((num 1) (den 1)))))))))))
              ((clause ()) (reason (Rup (hints (0 1 2 3 4)))))))
            (contradiction 5))))))))
     (conclusion 7))
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
  [%expect {|
    ((assumptions
      (((name ()) (formula (Not (Eq Bool Int))))
       ((name ()) (formula (Not (Eq Bool Float))))
       ((name ()) (formula (Not (Eq Int Float))))
       ((name ()) (formula (Or ((Eq (Var x) (Var y)) (Eq (Var y) (Var z))))))
       ((name ()) (formula (Not (Eq (Var x) (Var y)))))
       ((name ()) (formula (Not (Eq (Var y) (Var z)))))))
     (steps
      (((name ()) (conclusion (Not (Eq Bool Int)))
        (justification (Assumption 0)))
       ((name ()) (conclusion (Not (Eq Bool Float)))
        (justification (Assumption 1)))
       ((name ()) (conclusion (Not (Eq Int Float)))
        (justification (Assumption 2)))
       ((name ()) (conclusion (Or ((Eq (Var x) (Var y)) (Eq (Var y) (Var z)))))
        (justification (Assumption 3)))
       ((name ()) (conclusion (Not (Eq (Var x) (Var y))))
        (justification (Assumption 4)))
       ((name ()) (conclusion (Not (Eq (Var y) (Var z))))
        (justification (Assumption 5)))
       ((name ()) (conclusion False)
        (justification
         (By_refutation (premises (0 1 2 3 4 5))
          (refutation
           ((inputs
             ((Not (Eq Bool Int)) (Not (Eq Bool Float)) (Not (Eq Int Float))
              (Or ((Eq (Var x) (Var y)) (Eq (Var y) (Var z))))
              (Not (Eq (Var x) (Var y))) (Not (Eq (Var y) (Var z))) (Not False)))
            (extensions
             (((id 0)
               (definition
                (Or
                 ((Atom (Theory (Eq ((Var x) (Var y)))))
                  (Atom (Theory (Eq ((Var y) (Var z)))))))))))
            (steps
             (((clause
                (((atom (Theory (Eq ((Var x) (Var y))))) (positive true))
                 ((atom (Theory (Eq ((Var y) (Var z))))) (positive true))
                 ((atom (Extension 0)) (positive false))))
               (reason (Extension_definition 0)))
              ((clause (((atom (Extension 0)) (positive true))))
               (reason
                (Input_clause
                 ((input 3) (literal ((atom (Extension 0)) (positive true)))))))
              ((clause
                (((atom (Theory (Eq ((Var x) (Var y))))) (positive false))))
               (reason
                (Input_clause
                 ((input 4)
                  (literal
                   ((atom (Theory (Eq ((Var x) (Var y))))) (positive false)))))))
              ((clause
                (((atom (Theory (Eq ((Var y) (Var z))))) (positive false))))
               (reason
                (Input_clause
                 ((input 5)
                  (literal
                   ((atom (Theory (Eq ((Var y) (Var z))))) (positive false)))))))
              ((clause ()) (reason (Rup (hints (1 2 3 0)))))))
            (contradiction 4))))))))
     (conclusion 6))
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
  [%expect {|
    ((assumptions
      (((name ()) (formula (Not (Eq Bool Int))))
       ((name ()) (formula (Not (Eq Bool Float))))
       ((name ()) (formula (Not (Eq Int Float))))
       ((name ()) (formula (Or ((Eq (Var x) (Var y)) (Eq (Var x) (Var z))))))
       ((name ()) (formula (Not (Eq (Var x) (Var y)))))
       ((name ()) (formula (La_compare (Var x) Ge (La_const ((num 5) (den 1))))))
       ((name ()) (formula (La_compare (Var z) Le (La_const ((num 3) (den 1))))))))
     (steps
      (((name ()) (conclusion (Not (Eq Bool Int)))
        (justification (Assumption 0)))
       ((name ()) (conclusion (Not (Eq Bool Float)))
        (justification (Assumption 1)))
       ((name ()) (conclusion (Not (Eq Int Float)))
        (justification (Assumption 2)))
       ((name ()) (conclusion (Or ((Eq (Var x) (Var y)) (Eq (Var x) (Var z)))))
        (justification (Assumption 3)))
       ((name ()) (conclusion (Not (Eq (Var x) (Var y))))
        (justification (Assumption 4)))
       ((name ())
        (conclusion (La_compare (Var x) Ge (La_const ((num 5) (den 1)))))
        (justification (Assumption 5)))
       ((name ())
        (conclusion (La_compare (Var z) Le (La_const ((num 3) (den 1)))))
        (justification (Assumption 6)))
       ((name ()) (conclusion False)
        (justification
         (By_refutation (premises (0 1 2 3 4 5 6))
          (refutation
           ((inputs
             ((Not (Eq Bool Int)) (Not (Eq Bool Float)) (Not (Eq Int Float))
              (Or ((Eq (Var x) (Var y)) (Eq (Var x) (Var z))))
              (Not (Eq (Var x) (Var y)))
              (La_compare (Var x) Ge (La_const ((num 5) (den 1))))
              (La_compare (Var z) Le (La_const ((num 3) (den 1)))) (Not False)))
            (extensions
             (((id 0)
               (definition
                (Or
                 ((Atom (Theory (Eq ((Var x) (Var y)))))
                  (Atom (Theory (Eq ((Var x) (Var z)))))))))))
            (steps
             (((clause
                (((atom (Theory (Eq ((Var x) (Var y))))) (positive true))
                 ((atom (Theory (Eq ((Var x) (Var z))))) (positive true))
                 ((atom (Extension 0)) (positive false))))
               (reason (Extension_definition 0)))
              ((clause (((atom (Extension 0)) (positive true))))
               (reason
                (Input_clause
                 ((input 3) (literal ((atom (Extension 0)) (positive true)))))))
              ((clause
                (((atom (Theory (Eq ((Var x) (Var y))))) (positive false))))
               (reason
                (Input_clause
                 ((input 4)
                  (literal
                   ((atom (Theory (Eq ((Var x) (Var y))))) (positive false)))))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num -5) (den 1))))))
                  (positive true))))
               (reason
                (Input_clause
                 ((input 5)
                  (literal
                   ((atom
                     (Theory
                      (Le
                       (((coeffs ((x ((num -1) (den 1)))))
                         (const ((num 0) (den 1))))
                        ((num -5) (den 1))))))
                    (positive true)))))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((z ((num 1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 3) (den 1))))))
                  (positive true))))
               (reason
                (Input_clause
                 ((input 6)
                  (literal
                   ((atom
                     (Theory
                      (Le
                       (((coeffs ((z ((num 1) (den 1)))))
                         (const ((num 0) (den 1))))
                        ((num 3) (den 1))))))
                    (positive true)))))))
              ((clause
                (((atom (Theory (Eq ((Var x) (Var z))))) (positive false))
                 ((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num 1) (den 1))) (z ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 0) (den 1))))))
                  (positive true))))
               (reason
                (Theory_lemma
                 (Bare_var_eq
                  (Equality_implies_le (left x) (right z)
                   (direction Left_le_right))))))
              ((clause
                (((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num -5) (den 1))))))
                  (positive false))
                 ((atom
                   (Theory
                    (Le
                     (((coeffs ((x ((num 1) (den 1))) (z ((num -1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 0) (den 1))))))
                  (positive false))
                 ((atom
                   (Theory
                    (Le
                     (((coeffs ((z ((num 1) (den 1)))))
                       (const ((num 0) (den 1))))
                      ((num 3) (den 1))))))
                  (positive false))))
               (reason
                (Theory_lemma
                 (Linear_arithmetic
                  ((combination
                    (((clause_literal 1) (coefficient ((num 1) (den 1))))
                     ((clause_literal 0) (coefficient ((num 1) (den 1))))
                     ((clause_literal 2) (coefficient ((num 1) (den 1)))))))))))
              ((clause ()) (reason (Rup (hints (1 2 3 4 6 0 5)))))))
            (contradiction 7))))))))
     (conclusion 7))
    |}]
;;
