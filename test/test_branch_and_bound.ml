open! Core
open! Feel.Import
open! Theory

let x = Tvar.of_string "x"
let y = Tvar.of_string "y"

(* ----- Unit-level: exercise Branch_and_bound.t directly ----- *)

let le tvar c : Branch_and_bound.Atom.t = `Le (Linear_expr.var tvar, Q.of_int c)

let ge tvar c : Branch_and_bound.Atom.t =
  `Le (Linear_expr.neg (Linear_expr.var tvar), Q.of_int (-c))
;;

let is_int tvar : Branch_and_bound.Atom.t =
  `Type_eq (Type_expr.Var tvar, Base Int)
;;

let print_lemma t =
  print_s
    [%sexp
      (Branch_and_bound.maybe_get_lemma t
       : [ `Consistent | `Lemma of (Branch_and_bound.Atom.t * bool) list ])]
;;

let%expect_test "satisfiable linear system stays `Consistent" =
  let t = Branch_and_bound.create () in
  Branch_and_bound.assert_atom t ~decision_level:0 ~atom:(le x 10) ~value:true;
  Branch_and_bound.assert_atom t ~decision_level:0 ~atom:(ge x 0) ~value:true;
  Branch_and_bound.assert_atom t ~decision_level:0 ~atom:(le y 5) ~value:true;
  print_lemma t;
  [%expect {| Consistent |}]
;;

let%expect_test "x <= 3 and x >= 5 conflict; lemma negates both" =
  let t = Branch_and_bound.create () in
  Branch_and_bound.assert_atom t ~decision_level:0 ~atom:(le x 3) ~value:true;
  Branch_and_bound.assert_atom t ~decision_level:0 ~atom:(ge x 5) ~value:true;
  print_lemma t;
  [%expect
    {|
    (Lemma
     (((Le
        (((coeffs ((x ((num -1) (den 1))))) (const ((num 0) (den 1))))
         ((num -5) (den 1))))
       false)
      ((Le
        (((coeffs ((x ((num 1) (den 1))))) (const ((num 0) (den 1))))
         ((num 3) (den 1))))
       false)))
    |}]
;;

let%expect_test "integral var with non-integral relaxed solution yields a \
                 case-split lemma"
  =
  let t = Branch_and_bound.create () in
  Branch_and_bound.assert_atom t ~decision_level:0 ~atom:(is_int x) ~value:true;
  (* 2x = 3, i.e. x <= 1.5 and x >= 1.5, forces x to a non-integral value *)
  Branch_and_bound.assert_atom
    t
    ~decision_level:0
    ~atom:(`Le (Linear_expr.scale (Q.of_int 2) (Linear_expr.var x), Q.of_int 3))
    ~value:true;
  Branch_and_bound.assert_atom
    t
    ~decision_level:0
    ~atom:
      (`Le
        ( Linear_expr.neg (Linear_expr.scale (Q.of_int 2) (Linear_expr.var x))
        , Q.of_int (-3) ))
    ~value:true;
  print_lemma t;
  [%expect
    {|
    (Lemma
     (((Le
        (((coeffs ((x ((num 1) (den 1))))) (const ((num 0) (den 1))))
         ((num 1) (den 1))))
       true)
      ((Le
        (((coeffs ((x ((num -1) (den 1))))) (const ((num 0) (den 1))))
         ((num -2) (den 1))))
       true)))
    |}];
  (* Committing to the [x <= 1] branch of the case split should conflict with
     [2x = 3] (x = 1.5), since no integer x satisfies both. *)
  Branch_and_bound.assert_atom
    t
    ~decision_level:1
    ~atom:(`Le (Linear_expr.var x, Q.of_int 1))
    ~value:true;
  print_lemma t;
  [%expect
    {|
    (Lemma
     (((Le
        (((coeffs ((x ((num -2) (den 1))))) (const ((num 0) (den 1))))
         ((num -3) (den 1))))
       false)
      ((Le
        (((coeffs ((x ((num 1) (den 1))))) (const ((num 0) (den 1))))
         ((num 1) (den 1))))
       false)))
    |}]
;;

let%expect_test "undo retracts a constraint and restores consistency" =
  let t = Branch_and_bound.create () in
  Branch_and_bound.assert_atom t ~decision_level:0 ~atom:(le x 3) ~value:true;
  Branch_and_bound.assert_atom t ~decision_level:1 ~atom:(ge x 5) ~value:true;
  print_lemma t;
  [%expect
    {|
    (Lemma
     (((Le
        (((coeffs ((x ((num -1) (den 1))))) (const ((num 0) (den 1))))
         ((num -5) (den 1))))
       false)
      ((Le
        (((coeffs ((x ((num 1) (den 1))))) (const ((num 0) (den 1))))
         ((num 3) (den 1))))
       false)))
    |}];
  Branch_and_bound.undo t ~to_decision_level_excl:0;
  print_lemma t;
  [%expect {| Consistent |}]
;;

(* ----- Solver-level: exercise Le/Type_eq through Solver.assert_formula ----- *)

let solver_le tvar c : Formula.t = Atom (`Le (Linear_expr.var tvar, Q.of_int c))

let solver_ge tvar c : Formula.t =
  Atom (`Le (Linear_expr.neg (Linear_expr.var tvar), Q.of_int (-c)))
;;

let solver_is_int tvar : Formula.t =
  Atom (`Type_eq (Type_expr.Var tvar, Base Int))
;;

let print_result (result : Solver_result.t) =
  print_s [%sexp (result : Solver_result.t)]
;;

let assert_ok solver formula =
  match Solver.assert_formula solver formula with
  | `Ok -> ()
  | `Unsat _ -> print_endline "UNSAT (at assert time)"
;;

let%expect_test "solver: satisfiable linear system" =
  let solver = Solver.create () in
  assert_ok solver (solver_le x 10);
  assert_ok solver (solver_ge x 0);
  assert_ok solver (solver_le y 5);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true) (true) (true)))) |}]
;;

let%expect_test "solver: x <= 3 and x >= 5 is unsat via the simplex theory" =
  let solver = Solver.create () in
  assert_ok solver (solver_le x 3);
  assert_ok solver (solver_ge x 5);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not
           (Atom
            (Le
             (((coeffs ((x ((num -1) (den 1))))) (const ((num 0) (den 1))))
              ((num -5) (den 1))))))
          (Not
           (Atom
            (Le
             (((coeffs ((x ((num 1) (den 1))))) (const ((num 0) (den 1))))
              ((num 3) (den 1)))))))))
       (Asserted
        (Atom
         (Le
          (((coeffs ((x ((num -1) (den 1))))) (const ((num 0) (den 1))))
           ((num -5) (den 1))))))
       (Asserted
        (Atom
         (Le
          (((coeffs ((x ((num 1) (den 1))))) (const ((num 0) (den 1))))
           ((num 3) (den 1)))))))))
    |}]
;;

let%expect_test "solver: integral var branches on a non-integral relaxed \
                 solution"
  =
  let solver = Solver.create () in
  assert_ok solver (solver_is_int x);
  (* 2x = 3 forces x = 1.5, which is non-integral, and no integer x can satisfy
     it: unsat. *)
  assert_ok
    solver
    (Atom (`Le (Linear_expr.scale (Q.of_int 2) (Linear_expr.var x), Q.of_int 3)));
  assert_ok
    solver
    (Atom
       (`Le
         ( Linear_expr.neg (Linear_expr.scale (Q.of_int 2) (Linear_expr.var x))
         , Q.of_int (-3) )));
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not
           (Atom
            (Le
             (((coeffs ((x ((num 1) (den 1))))) (const ((num 0) (den 1))))
              ((num 1) (den 1))))))
          (Not
           (Atom
            (Le
             (((coeffs ((x ((num -2) (den 1))))) (const ((num 0) (den 1))))
              ((num -3) (den 1)))))))))
       (Theory_lemma
        (Or
         ((Atom
           (Le
            (((coeffs ((x ((num 1) (den 1))))) (const ((num 0) (den 1))))
             ((num 1) (den 1)))))
          (Atom
           (Le
            (((coeffs ((x ((num -1) (den 1))))) (const ((num 0) (den 1))))
             ((num -2) (den 1))))))))
       (Asserted
        (Atom
         (Le
          (((coeffs ((x ((num -2) (den 1))))) (const ((num 0) (den 1))))
           ((num -3) (den 1)))))))))
    |}]
;;

let%expect_test "solver: push/pop retracts a simplex conflict" =
  let solver = Solver.create () in
  assert_ok solver (solver_le x 3);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true)))) |}];
  Solver.push solver;
  assert_ok solver (solver_ge x 5);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not
           (Atom
            (Le
             (((coeffs ((x ((num -1) (den 1))))) (const ((num 0) (den 1))))
              ((num -5) (den 1))))))
          (Not
           (Atom
            (Le
             (((coeffs ((x ((num 1) (den 1))))) (const ((num 0) (den 1))))
              ((num 3) (den 1)))))))))
       (Asserted
        (Atom
         (Le
          (((coeffs ((x ((num -1) (den 1))))) (const ((num 0) (den 1))))
           ((num -5) (den 1))))))
       (Asserted
        (Atom
         (Le
          (((coeffs ((x ((num 1) (den 1))))) (const ((num 0) (den 1))))
           ((num 3) (den 1)))))))))
    |}];
  Solver.pop solver;
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true) (false) (false)))) |}]
;;
