open! Core
open! Feel.Import
open! Theory

let x = `Var (Tvar.of_string "x")
let y = `Var (Tvar.of_string "y")
let z = `Var (Tvar.of_string "z")
let f arg : Term.t = `App (~function_:(Tvar.of_string "f"), ~args:[ arg ])
let eq a b : Formula.t = Atom (`Eq (a, b))
let neq a b : Formula.t = Not (eq a b)

let print_result (result : Solver_result.t) =
  print_s [%sexp (result : Solver_result.t)]
;;

let print_feel_result (result : Feel.Sat_result.t) =
  print_s [%sexp (result : Feel.Sat_result.t)]
;;

let assert_ok solver formula =
  match Solver.assert_formula solver formula with
  | `Ok -> ()
  | `Unsat _ -> print_endline "UNSAT (at assert time)"
;;

(* ----- Term/Atom basics ----- *)

(* Renders a [Term.t] using variable/function names instead of raw interned
   ints, for readable expect-test output. *)
let rec sexp_of_term (term : Term.t) : Sexp.t =
  match term with
  | `Var v -> List [ Atom "Var"; Atom (Tvar.to_string v) ]
  | `App (~function_, ~args) ->
    List
      [ Atom "App"
      ; Atom (Tvar.to_string function_)
      ; List (List.map args ~f:sexp_of_term)
      ]
;;

let sexp_of_atom = [%sexp_of: Atom.t]

let%expect_test "Atom.normalize orders Eq sides canonically" =
  let a = `Eq (x, y) in
  let b = `Eq (y, x) in
  print_s [%sexp (sexp_of_atom (Atom.normalize a) : Sexp.t)];
  print_s [%sexp (sexp_of_atom (Atom.normalize b) : Sexp.t)];
  [%expect {|
    (Eq ((Var x) (Var y)))
    (Eq ((Var x) (Var y)))
    |}]
;;

let%expect_test "Term/Atom sexp round trip" =
  let term : Term.t = f x in
  print_s [%sexp (sexp_of_term term : Sexp.t)];
  [%expect {| (App f ((Var x))) |}]
;;

(* ----- Formula / Tseitin correctness ----- *)

let%expect_test "Tseitin: simple propositional formula matches truth table" =
  let a = `Eq (x, x) in
  let b = `Eq (y, y) in
  let formula : Formula.t = And [ Atom a; Or [ Atom b; Not (Atom a) ] ] in
  let encoding = Formula.Encoding.create () in
  let clauses = Formula.encode encoding formula in
  let solver = Feel.Solver.create () in
  List.iter clauses ~f:(fun clause ->
    ignore (Feel.Solver.add_clause solver ~clause : [ `Ok | `Unsat of _ ]));
  let result = Feel.Solver.solve solver in
  let var_a = Formula.Encoding.sat_var_for_atom encoding a in
  let var_b = Formula.Encoding.sat_var_for_atom encoding b in
  (match result with
   | Unsat _ -> print_endline "UNSAT"
   | Sat { assignments } ->
     print_s
       [%message
         "SAT"
           (var_a : int)
           (var_b : int)
           ~a:(assignments.(var_a) : bool option)
           ~b:(assignments.(var_b) : bool option)]);
  (* a must be true; b must also be true (since a is true, [Or [b; not a]]
     forces b) *)
  [%expect {| (SAT (var_a 1) (var_b 2) (a (true)) (b (true))) |}]
;;

let%expect_test "Tseitin: atom -> sat_var mapping is stable across encode calls"
  =
  let a = `Eq (x, y) in
  let encoding = Formula.Encoding.create () in
  let (_ : int array list) = Formula.encode encoding (Atom a) in
  let var1 = Formula.Encoding.sat_var_for_atom encoding a in
  let (_ : int array list) = Formula.encode encoding (Not (Atom a)) in
  let var2 = Formula.Encoding.sat_var_for_atom encoding a in
  print_s [%sexp (var1 : int), (var2 : int)];
  [%expect {| (1 1) |}]
;;

let%expect_test "Tseitin: True/False and double negation" =
  let encoding = Formula.Encoding.create () in
  let solver = Feel.Solver.create () in
  let assert_formula formula =
    List.iter (Formula.encode encoding formula) ~f:(fun clause ->
      ignore (Feel.Solver.add_clause solver ~clause : [ `Ok | `Unsat of _ ]))
  in
  assert_formula True;
  assert_formula (Not (Not True));
  print_feel_result (Feel.Solver.solve solver);
  [%expect {| (Sat (assignments (() (true)))) |}]
;;

let%expect_test "Tseitin: False is unsatisfiable" =
  let encoding = Formula.Encoding.create () in
  let solver = Feel.Solver.create () in
  let result =
    List.fold (Formula.encode encoding False) ~init:`Ok ~f:(fun acc clause ->
      match acc with
      | `Unsat _ -> acc
      | `Ok -> Feel.Solver.add_clause solver ~clause)
  in
  (match result with
   | `Ok -> print_endline "ok"
   | `Unsat _ -> print_endline "UNSAT (at assert time)");
  [%expect {| UNSAT (at assert time) |}]
;;

let%expect_test "Tseitin: And [] is True, Or [] is False" =
  let encoding = Formula.Encoding.create () in
  let solver = Feel.Solver.create () in
  let assert_formula formula =
    List.iter (Formula.encode encoding formula) ~f:(fun clause ->
      ignore (Feel.Solver.add_clause solver ~clause : [ `Ok | `Unsat of _ ]))
  in
  assert_formula (And []);
  assert_formula (Not (Or []));
  print_feel_result (Feel.Solver.solve solver);
  [%expect {| (Sat (assignments (() (true) (false)))) |}]
;;

let%expect_test "Tseitin: Or [] (False) contradicts True" =
  let encoding = Formula.Encoding.create () in
  let solver = Feel.Solver.create () in
  let result =
    List.fold (Formula.encode encoding (Or [])) ~init:`Ok ~f:(fun acc clause ->
      match acc with
      | `Unsat _ -> acc
      | `Ok -> Feel.Solver.add_clause solver ~clause)
  in
  (match result with
   | `Ok -> print_endline "ok"
   | `Unsat _ -> print_endline "UNSAT (at assert time)");
  [%expect {| UNSAT (at assert time) |}]
;;

(* ----- theory/solver.ml end-to-end EUF tests ----- *)

let%expect_test "pure boolean formula (no theory atoms)" =
  let a = `Eq (x, x) in
  let b = `Eq (y, y) in
  let solver = Solver.create () in
  (* (a \/ b) /\ (~a \/ b) /\ (a \/ ~b) => b must be true *)
  assert_ok solver (Or [ Atom a; Atom b ]);
  assert_ok solver (Or [ Not (Atom a); Atom b ]);
  assert_ok solver (Or [ Atom a; Not (Atom b) ]);
  print_result (Solver.solve solver);
  [%expect
    {| (Sat (assignments (() (false) (true) (true) (true) (true) (true)))) |}]
;;

let%expect_test "EUF: transitivity violation is unsat" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  assert_ok solver (neq y z);
  assert_ok solver (eq x z);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Atom (Eq ((Var x) (Var z))))) (Not (Atom (Eq ((Var x) (Var y)))))
          (Atom (Eq ((Var y) (Var z)))))))
       (Asserted (Atom (Eq ((Var x) (Var z)))))
       (Asserted (Atom (Eq ((Var x) (Var y)))))
       (Asserted (Not (Atom (Eq ((Var y) (Var z)))))))))
    |}]
;;

let%expect_test "EUF: congruence conflict (f(x) <> f(y) with x = y)" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  assert_ok solver (neq (f x) (f y));
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Atom (Eq ((Var x) (Var y)))))
          (Atom
           (Eq
            ((App ((~function_ f) (~args ((Var x)))))
             (App ((~function_ f) (~args ((Var y)))))))))))
       (Asserted (Atom (Eq ((Var x) (Var y)))))
       (Asserted
        (Not
         (Atom
          (Eq
           ((App ((~function_ f) (~args ((Var x)))))
            (App ((~function_ f) (~args ((Var y)))))))))))))
    |}]
;;

let%expect_test "EUF: congruence positive propagation (x = y forces f(x) = \
                 f(y))"
  =
  let solver = Solver.create () in
  (* assert f(x) <> f(y) first, then x = y -- should still detect conflict *)
  assert_ok solver (neq (f x) (f y));
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Atom (Eq ((Var x) (Var y)))))
          (Atom
           (Eq
            ((App ((~function_ f) (~args ((Var x)))))
             (App ((~function_ f) (~args ((Var y)))))))))))
       (Asserted (Atom (Eq ((Var x) (Var y)))))
       (Asserted
        (Not
         (Atom
          (Eq
           ((App ((~function_ f) (~args ((Var x)))))
            (App ((~function_ f) (~args ((Var y)))))))))))))
    |}]
;;

let%expect_test "EUF: satisfiable case" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  assert_ok solver (neq y z);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true) (false)))) |}]
;;

(* ----- Incremental assert_formula + solve ----- *)

(* Asserting a unit clause that directly contradicts an already-true literal at
   decision level 0 is rejected by [assert_formula] itself (mirroring
   [Feel.Solver.add_clause]'s reject-on-conflict semantics): the conflicting
   clause is never added, so the solver's state is unchanged and the next
   [solve] still reports the prior (now-stale) result. *)
let%expect_test "incremental: asserting a contradicting formula after solve" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true)))) |}];
  assert_ok solver (neq x y);
  print_result (Solver.solve solver);
  [%expect
    {|
    UNSAT (at assert time)
    (Sat (assignments (() (false) (true))))
    |}]
;;

let%expect_test "incremental: new EUF atoms registered after a solve" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true)))) |}];
  (* introduce brand-new terms/atoms involving f, after solving once *)
  assert_ok solver (neq (f x) (f y));
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Atom (Eq ((Var x) (Var y)))))
          (Atom
           (Eq
            ((App ((~function_ f) (~args ((Var x)))))
             (App ((~function_ f) (~args ((Var y)))))))))))
       (Asserted (Atom (Eq ((Var x) (Var y)))))
       (Asserted
        (Not
         (Atom
          (Eq
           ((App ((~function_ f) (~args ((Var x)))))
            (App ((~function_ f) (~args ((Var y)))))))))))))
    |}]
;;

(* ----- Push/pop ----- *)

let%expect_test "push/pop: retracting a contradicting constraint" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true)))) |}];
  Solver.push solver;
  assert_ok solver (neq x y);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Asserted (Not (Atom (Eq ((Var x) (Var y))))))
       (Asserted (Atom (Eq ((Var x) (Var y))))))))
    |}];
  Solver.pop solver;
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true) (false)))) |}]
;;

let%expect_test "push/pop: nested scopes" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true)))) |}];
  Solver.push solver;
  assert_ok solver (neq y z);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true) (true) (false)))) |}];
  Solver.push solver;
  assert_ok solver (eq x z);
  (* x=y, y<>z, x=z is a transitivity violation *)
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Asserted (Not (Atom (Eq ((Var y) (Var z))))))
       (Theory_lemma
        (Or
         ((Atom (Eq ((Var y) (Var z)))) (Not (Atom (Eq ((Var x) (Var y)))))
          (Not (Atom (Eq ((Var x) (Var z))))))))
       (Asserted (Atom (Eq ((Var x) (Var y)))))
       (Asserted (Atom (Eq ((Var x) (Var z))))))))
    |}];
  Solver.pop solver;
  (* back to: x=y, y<>z -- satisfiable *)
  print_result (Solver.solve solver);
  [%expect
    {| (Sat (assignments (() (false) (true) (true) (false) (false) (false)))) |}];
  Solver.pop solver;
  (* back to: x=y only -- satisfiable *)
  print_result (Solver.solve solver);
  [%expect
    {| (Sat (assignments (() (false) (true) (true) (false) (false) (false)))) |}]
;;

let%expect_test "push/pop: Tseitin-encoded Or inside scope appears in unsat \
                 core"
  =
  (* An Or formula creates Tseitin auxiliary clauses. When asserted inside a
     push scope those clauses are guarded by an activation literal, making them
     multi-literal. The unsat core must still show the original Or formula. *)
  let solver = Solver.create () in
  Solver.push solver;
  assert_ok solver (Or [ eq x y; eq y z ]);
  assert_ok solver (neq x y);
  assert_ok solver (neq y z);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Asserted (Not (Atom (Eq ((Var x) (Var y))))))
       (Asserted
        (Or ((Atom (Eq ((Var x) (Var y)))) (Atom (Eq ((Var y) (Var z)))))))
       (Asserted (Not (Atom (Eq ((Var y) (Var z)))))))))
    |}]
;;

let%expect_test "push/pop: EUF congruence conflict inside a scope is retracted \
                 on pop"
  =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true)))) |}];
  Solver.push solver;
  assert_ok solver (neq (f x) (f y));
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Atom (Eq ((Var x) (Var y)))))
          (Atom
           (Eq
            ((App ((~function_ f) (~args ((Var x)))))
             (App ((~function_ f) (~args ((Var y)))))))))))
       (Asserted (Atom (Eq ((Var x) (Var y)))))
       (Asserted
        (Not
         (Atom
          (Eq
           ((App ((~function_ f) (~args ((Var x)))))
            (App ((~function_ f) (~args ((Var y)))))))))))))
    |}];
  Solver.pop solver;
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true) (false) (true)))) |}];
  (* solving again should remain consistent *)
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true) (false) (true)))) |}]
;;

(* ----- Stats passthrough ----- *)

let%expect_test "stats passthrough" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  ignore (Solver.solve solver : Solver_result.t);
  let stats = Solver.stats solver in
  print_s [%message "" ~iterations_nonneg:(stats.#iterations >= 0 : bool)];
  [%expect {| (iterations_nonneg true) |}]
;;

(* ----- Type theory (Has_type / Type_eq) ----- *)

let xv = Tvar.of_string "x"
let yv = Tvar.of_string "y"
let int_type : Type_expr.t = Base Int
let float_type : Type_expr.t = Base Float
let array_ctor = Tvar.of_string "Array"
let array_of (elem : Type_expr.t) : Type_expr.t = App (array_ctor, [ elem ])
let type_eq te1 te2 : Formula.t = Atom (`Type_eq (te1, te2))

let%expect_test "Has_type: basic assert and get_type" =
  let solver = Solver.create () in
  Solver.assert_type solver xv int_type;
  print_s [%sexp (Solver.get_type solver xv : Type_expr.t option)];
  ignore (Solver.solve solver : Solver_result.t);
  print_s [%sexp (Solver.get_type solver xv : Type_expr.t option)];
  [%expect {|
    ((Base Int))
    ((Base Int))
    |}]
;;

let%expect_test "Has_type: conflicting ground types are unsat" =
  let solver = Solver.create () in
  Solver.assert_type solver xv int_type;
  Solver.assert_type solver xv float_type;
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Atom (Has_type (x (Base Float)))))
          (Not (Atom (Has_type (x (Base Int))))))))
       (Asserted (Atom (Has_type (x (Base Float)))))
       (Asserted (Atom (Has_type (x (Base Int))))))))
    |}]
;;

let%expect_test "Has_type: two variables can have different types" =
  let solver = Solver.create () in
  Solver.assert_type solver xv int_type;
  Solver.assert_type solver yv float_type;
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true) (true)))) |}]
;;

let%expect_test "Has_type: structural conflict (Array vs Int)" =
  let a = Tvar.of_string "a" in
  let solver = Solver.create () in
  Solver.assert_type solver xv (array_of (Var a));
  Solver.assert_type solver xv int_type;
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Atom (Has_type (x (Base Int)))))
          (Not (Atom (Has_type (x (App Array ((Var a))))))))))
       (Asserted (Atom (Has_type (x (Base Int)))))
       (Asserted (Atom (Has_type (x (App Array ((Var a))))))))))
    |}]
;;

let%expect_test "Has_type: same constructor, different type args — sat without \
                 TypeEq constraints"
  =
  let a = Tvar.of_string "a" in
  let b = Tvar.of_string "b" in
  let solver = Solver.create () in
  (* Array('a) and Array('b) have the same head — no top-level conflict *)
  Solver.assert_type solver xv (array_of (Var a));
  Solver.assert_type solver xv (array_of (Var b));
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true) (true)))) |}]
;;

let%expect_test "Has_type: push/pop retracts type conflict" =
  let solver = Solver.create () in
  Solver.assert_type solver xv int_type;
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true)))) |}];
  Solver.push solver;
  Solver.assert_type solver xv float_type;
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Atom (Has_type (x (Base Float)))))
          (Not (Atom (Has_type (x (Base Int))))))))
       (Asserted (Atom (Has_type (x (Base Float)))))
       (Asserted (Atom (Has_type (x (Base Int))))))))
    |}];
  Solver.pop solver;
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true) (false) (false)))) |}]
;;

let%expect_test "Has_type: get_type reflects pushed/popped state" =
  let solver = Solver.create () in
  Solver.assert_type solver xv int_type;
  ignore (Solver.solve solver : Solver_result.t);
  print_s [%sexp (Solver.get_type solver xv : Type_expr.t option)];
  [%expect {| ((Base Int)) |}];
  Solver.push solver;
  Solver.assert_type solver xv float_type;
  (* conflicting, but get_type reflects the last literal asserted *)
  ignore (Solver.solve solver : Solver_result.t);
  print_s [%sexp (Solver.get_type solver xv : Type_expr.t option)];
  [%expect {| ((Base Float)) |}];
  Solver.pop solver;
  ignore (Solver.solve solver : Solver_result.t);
  print_s [%sexp (Solver.get_type solver xv : Type_expr.t option)];
  [%expect {| ((Base Int)) |}]
;;

let%expect_test "Type_eq: TypeEq(a, Int) and TypeEq(a, Float) conflict via \
                 type-level EUF"
  =
  let a = Tvar.of_string "a" in
  let solver = Solver.create () in
  assert_ok solver (type_eq (Var a) int_type);
  assert_ok solver (type_eq (Var a) float_type);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Atom (Type_eq ((Base Float) (Var a)))))
          (Not (Atom (Type_eq ((Base Int) (Var a)))))
          (Atom (Type_eq ((Base Int) (Base Float)))))))
       (Asserted (Atom (Type_eq ((Var a) (Base Float)))))
       (Asserted (Atom (Type_eq ((Var a) (Base Int)))))
       (Asserted (Not (Atom (Type_eq ((Base Int) (Base Float)))))))))
    |}]
;;

let%expect_test "Type_eq: TypeEq(a, Int) alone is sat" =
  let a = Tvar.of_string "a" in
  let solver = Solver.create () in
  assert_ok solver (type_eq (Var a) int_type);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true)))) |}]
;;

let%expect_test "Type_eq: normalize makes Type_eq(a, b) = Type_eq(b, a)" =
  let a = Tvar.of_string "a" in
  let te = Type_expr.Var a in
  let atom1 = Atom.normalize (`Type_eq (te, int_type)) in
  let atom2 = Atom.normalize (`Type_eq (int_type, te)) in
  print_s [%sexp ([%equal: Atom.t] atom1 atom2 : bool)];
  [%expect {| true |}]
;;

(* ----- Arithmetic (branch-and-bound) ----- *)

let xv_int = Tvar.of_string "x"
let yv_int = Tvar.of_string "y"
let le tvar c : Formula.t = Atom (`Le (Linear_expr.var tvar, Q.of_int c))

let ge tvar c : Formula.t =
  Not (Atom (`Le (Linear_expr.var tvar, Q.of_int (c - 1))))
;;

let setup_int_solver () =
  let solver = Solver.create () in
  Solver.register_arithmetic_var solver xv_int Int;
  Solver.register_arithmetic_var solver yv_int Int;
  solver
;;

let%expect_test "arithmetic: x <= 5 is sat" =
  let solver = setup_int_solver () in
  assert_ok solver (le xv_int 5);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true)))) |}]
;;

let%expect_test "arithmetic: x <= 5 and x >= 7 is unsat" =
  let solver = setup_int_solver () in
  assert_ok solver (le xv_int 5);
  assert_ok solver (ge xv_int 7);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma (Or ((Not (Atom (Le ((coeffs ((x ((num 1) (den 1)))) (const ((num 0) (den 1)))) ((num 7) (den 1))))) (Not (Atom (Le ((coeffs ((x ((num 1) (den 1)))) (const ((num 0) (den 1)))) ((num 5) (den 1))))))))
        (Asserted (Not (Atom (Le ((coeffs ((x ((num 1) (den 1)))) (const ((num 0) (den 1)))) ((num 6) (den 1)))))))
        (Asserted (Atom (Le ((coeffs ((x ((num 1) (den 1)))) (const ((num 0) (den 1)))) ((num 5) (den 1)))))))))
    |}]
;;

let%expect_test "arithmetic: x <= 3 and x >= 3 is sat (x = 3)" =
  let solver = setup_int_solver () in
  assert_ok solver (le xv_int 3);
  assert_ok solver (ge xv_int 3);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true) (true)))) |}]
;;

let%expect_test "arithmetic: integer branching - x >= 0, x <= 1, x must be 0 \
                 or 1"
  =
  let solver = setup_int_solver () in
  assert_ok solver (ge xv_int 0);
  assert_ok solver (le xv_int 1);
  print_result (Solver.solve solver);
  [%expect {| (Sat (assignments (() (false) (true) (true)))) |}]
;;

(* [2x <= 3, x >= 0] has a fractional LP relaxation (x = 3/2); the integer
   solution must branch to [x <= 1] (since [x >= 2] is infeasible against the
   upper bound). Neither branch atom is asserted up front, so this exercises
   [Solver.sat_var_for_atom] allocating fresh sat vars for the branch lemma. *)
let%expect_test "arithmetic: fractional LP relaxation forces integer branching" =
  let solver = setup_int_solver () in
  let two_x_le_3 : Formula.t =
    Atom (`Le (Linear_expr.scale (Q.of_int 2) (Linear_expr.var xv_int), Q.of_int 3))
  in
  assert_ok solver two_x_le_3;
  assert_ok solver (ge xv_int 0);
  print_result (Solver.solve solver);
  [%expect
    {| (Sat (assignments (() (false) (true) (true) (true) (false)))) |}]
;;
