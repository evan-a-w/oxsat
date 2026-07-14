open! Core
open! Feel.Import
open! Theory

let x : Formula.any = Var (Tvar.of_string "x")
let y : Formula.any = Var (Tvar.of_string "y")
let z : Formula.any = Var (Tvar.of_string "z")
let f arg : Formula.any = App (Tvar.of_string "f", [ arg ])
let eq a b : Formula.any = Eq (a, b)
let neq a b : Formula.any = Not (eq a b)

let print_result (result : Solver_result.t) =
  match result with
  | Unsat _ -> print_s [%sexp (result : Solver_result.t)]
  | Sat { tvar_assignments } ->
    print_s [%message "Sat" (tvar_assignments : Tvar_assignment.t Tvar.Map.t)]
;;

let print_feel_result (result : Feel.Sat_result.t) =
  print_s [%sexp (result : Feel.Sat_result.t)]
;;

let assert_ok solver formula =
  match Or_error.ok_exn (Solver.assert_formula solver formula) with
  | `Ok -> ()
  | `Unsat _ -> print_endline "UNSAT (at assert time)"
;;

(* ----- Term/Atom basics ----- *)

let uf_x : Formula.any = Var (Tvar.of_string "x")
let uf_y : Formula.any = Var (Tvar.of_string "y")

let%expect_test "Atom.normalize orders Eq sides canonically" =
  let a = `Eq (uf_x, uf_y) in
  let b = `Eq (uf_y, uf_x) in
  print_s [%sexp (Atom.normalize a : Atom.t)];
  print_s [%sexp (Atom.normalize b : Atom.t)];
  [%expect {|
    (Eq ((Var x) (Var y)))
    (Eq ((Var x) (Var y)))
    |}]
;;

let%expect_test "Term/Atom sexp round trip" =
  let term : Formula.any = App (Tvar.of_string "f", [ uf_x ]) in
  print_s ([%sexp_of: Formula.any] term);
  [%expect {| (App f ((Var x))) |}]
;;

(* ----- Formula / Tseitin correctness ----- *)

let%expect_test "Tseitin: simple propositional formula matches truth table" =
  let a = eq x x in
  let b = eq y y in
  let formula : Formula.any = And [ a; Or [ b; Not a ] ] in
  let encoding = Encoding.create () in
  let clauses = Or_error.ok_exn (Encoding.encode encoding ~formula) in
  let solver = Feel.Solver.create () in
  List.iter clauses ~f:(fun clause ->
    ignore (Feel.Solver.add_clause solver ~clause : [ `Ok | `Unsat of _ ]));
  let result = Feel.Solver.solve solver in
  let var_a = Encoding.sat_var_for_atom encoding (`Eq (uf_x, uf_x)) in
  let var_b = Encoding.sat_var_for_atom encoding (`Eq (uf_y, uf_y)) in
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
  [%expect {| (SAT (var_a 1) (var_b 4) (a (true)) (b (true))) |}]
;;

let%expect_test "Tseitin: atom -> sat_var mapping is stable across encode calls"
  =
  let a = eq x y in
  let encoding = Encoding.create () in
  let (_ : int array list) =
    Or_error.ok_exn (Encoding.encode encoding ~formula:a)
  in
  let var1 = Encoding.sat_var_for_atom encoding (`Eq (uf_x, uf_y)) in
  let (_ : int array list) =
    Or_error.ok_exn (Encoding.encode encoding ~formula:(Not a))
  in
  let var2 = Encoding.sat_var_for_atom encoding (`Eq (uf_x, uf_y)) in
  print_s [%sexp (var1 : int), (var2 : int)];
  [%expect {| (1 1) |}]
;;

let%expect_test "Tseitin: True/False and double negation" =
  let encoding = Encoding.create () in
  let solver = Feel.Solver.create () in
  let assert_formula formula =
    List.iter
      (Or_error.ok_exn (Encoding.encode encoding ~formula))
      ~f:(fun clause ->
        ignore (Feel.Solver.add_clause solver ~clause : [ `Ok | `Unsat of _ ]))
  in
  assert_formula True;
  assert_formula (Not (Not True));
  print_feel_result (Feel.Solver.solve solver);
  [%expect {| (Sat (assignments (() (true)))) |}]
;;

let%expect_test "Tseitin: False is unsatisfiable" =
  let encoding = Encoding.create () in
  let solver = Feel.Solver.create () in
  let result =
    List.fold
      (Or_error.ok_exn (Encoding.encode encoding ~formula:False))
      ~init:`Ok
      ~f:(fun acc clause ->
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
  let encoding = Encoding.create () in
  let solver = Feel.Solver.create () in
  let assert_formula formula =
    List.iter
      (Or_error.ok_exn (Encoding.encode encoding ~formula))
      ~f:(fun clause ->
        ignore (Feel.Solver.add_clause solver ~clause : [ `Ok | `Unsat of _ ]))
  in
  assert_formula (And []);
  assert_formula (Not (Or []));
  print_feel_result (Feel.Solver.solve solver);
  [%expect {| (Sat (assignments (() (true) (false)))) |}]
;;

let%expect_test "Tseitin: Or [] (False) contradicts True" =
  let encoding = Encoding.create () in
  let solver = Feel.Solver.create () in
  let result =
    List.fold
      (Or_error.ok_exn (Encoding.encode encoding ~formula:(Or [])))
      ~init:`Ok
      ~f:(fun acc clause ->
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
  let a = eq x x in
  let b = eq y y in
  let solver = Solver.create () in
  (* (a \/ b) /\ (~a \/ b) /\ (a \/ ~b) => b must be true *)
  assert_ok solver (Or [ a; b ]);
  assert_ok solver (Or [ Not a; b ]);
  assert_ok solver (Or [ a; Not b ]);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Var x))) (numeric ()) (euf_repr ())))
       (y ((type_ ((Var y))) (numeric ()) (euf_repr ()))))))
    |}]
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
         ((Not (Eq (Var x) (Var z))) (Not (Eq (Var x) (Var y)))
          (Eq (Var y) (Var z)))))
       (Asserted (Eq (Var x) (Var z))) (Asserted (Eq (Var x) (Var y)))
       (Asserted (Not (Eq (Var y) (Var z)))))))
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
         ((Not (Eq (Var x) (Var y))) (Eq (App f ((Var x))) (App f ((Var y)))))))
       (Asserted (Eq (Var x) (Var y)))
       (Asserted (Not (Eq (App f ((Var x))) (App f ((Var y)))))))))
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
         ((Not (Eq (Var x) (Var y))) (Eq (App f ((Var x))) (App f ((Var y)))))))
       (Asserted (Eq (Var x) (Var y)))
       (Asserted (Not (Eq (App f ((Var x))) (App f ((Var y)))))))))
    |}]
;;

let%expect_test "EUF: satisfiable case" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  assert_ok solver (neq y z);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Var y))) (numeric ()) (euf_repr ())))
       (y
        ((type_ ())
         (numeric (((value ((num 0) (den 1))) (eps_coeff ((num 1) (den 1))))))
         (euf_repr ((Var x)))))
       (z
        ((type_ ())
         (numeric (((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))
         (euf_repr ()))))))
    |}]
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
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Var y))) (numeric ()) (euf_repr ())))
       (y ((type_ ()) (numeric ()) (euf_repr ((Var x))))))))
    |}];
  assert_ok solver (neq x y);
  print_result (Solver.solve solver);
  [%expect
    {|
    UNSAT (at assert time)
    (Sat
     (tvar_assignments
      ((x
        ((type_ ((Var y)))
         (numeric (((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))
         (euf_repr ())))
       (y
        ((type_ ())
         (numeric (((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))
         (euf_repr ((Var x))))))))
    |}]
;;

let%expect_test "incremental: new EUF atoms registered after a solve" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Var y))) (numeric ()) (euf_repr ())))
       (y ((type_ ()) (numeric ()) (euf_repr ((Var x))))))))
    |}];
  (* introduce brand-new terms/atoms involving f, after solving once *)
  assert_ok solver (neq (f x) (f y));
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Eq (Var x) (Var y))) (Eq (App f ((Var x))) (App f ((Var y)))))))
       (Asserted (Eq (Var x) (Var y)))
       (Asserted (Not (Eq (App f ((Var x))) (App f ((Var y)))))))))
    |}]
;;

(* ----- Push/pop ----- *)

let%expect_test "push/pop: retracting a contradicting constraint" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Var y))) (numeric ()) (euf_repr ())))
       (y ((type_ ()) (numeric ()) (euf_repr ((Var x))))))))
    |}];
  Solver.push solver;
  assert_ok solver (neq x y);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Asserted (Not (Eq (Var x) (Var y)))) (Asserted (Eq (Var x) (Var y))))))
    |}];
  Solver.pop solver;
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x
        ((type_ ((Var y)))
         (numeric (((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))
         (euf_repr ())))
       (y
        ((type_ ())
         (numeric (((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))
         (euf_repr ((Var x))))))))
    |}]
;;

let%expect_test "push/pop: nested scopes" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Var y))) (numeric ()) (euf_repr ())))
       (y ((type_ ()) (numeric ()) (euf_repr ((Var x))))))))
    |}];
  Solver.push solver;
  assert_ok solver (neq y z);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Var y))) (numeric ()) (euf_repr ())))
       (y
        ((type_ ())
         (numeric (((value ((num 0) (den 1))) (eps_coeff ((num 1) (den 1))))))
         (euf_repr ((Var x)))))
       (z
        ((type_ ())
         (numeric (((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))
         (euf_repr ()))))))
    |}];
  Solver.push solver;
  assert_ok solver (eq x z);
  (* x=y, y<>z, x=z is a transitivity violation *)
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Asserted (Not (Eq (Var y) (Var z))))
       (Theory_lemma
        (Or
         ((Eq (Type_var y) (Type_var z)) (Not (Eq (Type_var x) (Type_var y)))
          (Not (Eq (Type_var x) (Type_var z))))))
       (Asserted (Eq (Var x) (Var y))) (Asserted (Eq (Var x) (Var z))))))
    |}];
  Solver.pop solver;
  (* back to: x=y, y<>z -- satisfiable *)
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Var y))) (numeric ()) (euf_repr ())))
       (y
        ((type_ ())
         (numeric (((value ((num 0) (den 1))) (eps_coeff ((num 1) (den 1))))))
         (euf_repr ((Var x)))))
       (z
        ((type_ ())
         (numeric (((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))
         (euf_repr ()))))))
    |}];
  Solver.pop solver;
  (* back to: x=y only -- satisfiable *)
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Var y))) (numeric ()) (euf_repr ())))
       (y
        ((type_ ())
         (numeric (((value ((num 0) (den 1))) (eps_coeff ((num 1) (den 1))))))
         (euf_repr ((Var x)))))
       (z
        ((type_ ())
         (numeric (((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))
         (euf_repr ()))))))
    |}]
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
      ((Asserted (Not (Eq (Var y) (Var z))))
       (Asserted (Not (Eq (Var x) (Var y))))
       (Asserted (Or ((Eq (Var x) (Var y)) (Eq (Var y) (Var z))))))))
    |}]
;;

let%expect_test "push/pop: EUF congruence conflict inside a scope is retracted \
                 on pop"
  =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Var y))) (numeric ()) (euf_repr ())))
       (y ((type_ ()) (numeric ()) (euf_repr ((Var x))))))))
    |}];
  Solver.push solver;
  assert_ok solver (neq (f x) (f y));
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Eq (Var x) (Var y))) (Eq (App f ((Var x))) (App f ((Var y)))))))
       (Asserted (Eq (Var x) (Var y)))
       (Asserted (Not (Eq (App f ((Var x))) (App f ((Var y)))))))))
    |}];
  Solver.pop solver;
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Var y))) (numeric ()) (euf_repr ())))
       (y ((type_ ()) (numeric ()) (euf_repr ((Var x))))))))
    |}];
  (* solving again should remain consistent *)
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Var y))) (numeric ()) (euf_repr ())))
       (y ((type_ ()) (numeric ()) (euf_repr ((Var x))))))))
    |}]
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
let type_eq te1 te2 : Formula.any = Eq (te1, te2)
let ft_int : Formula.any = Int
let ft_float : Formula.any = Float
let ft_array (elem : Formula.any) : Formula.any = Type_app (array_ctor, [ elem ])

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
         ((Not (Eq (Type_var x) Float)) (Not (Eq (Type_var x) Int))
          (Eq Int Float))))
       (Asserted (Eq (Type_var x) Float)) (Asserted (Eq (Type_var x) Int))
       (Asserted (Not (Eq Int Float))))))
    |}]
;;

let%expect_test "Has_type: two variables can have different types" =
  let solver = Solver.create () in
  Solver.assert_type solver xv int_type;
  Solver.assert_type solver yv float_type;
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((Base Int))) (numeric ()) (euf_repr ())))
       (y ((type_ ((Base Float))) (numeric ()) (euf_repr ()))))))
    |}]
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
         ((Not (Eq (Type_var x) Int))
          (Not (Eq (Type_var x) (Type_app Array ((Type_var a))))))))
       (Asserted (Eq (Type_var x) Int))
       (Asserted (Eq (Type_var x) (Type_app Array ((Type_var a))))))))
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
  [%expect
    {|
    (Sat
     (tvar_assignments
      ((x ((type_ ((App Array ((Var b))))) (numeric ()) (euf_repr ()))))))
    |}]
;;

let%expect_test "Has_type: push/pop retracts type conflict" =
  let solver = Solver.create () in
  Solver.assert_type solver xv int_type;
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments ((x ((type_ ((Base Int))) (numeric ()) (euf_repr ()))))))
    |}];
  Solver.push solver;
  Solver.assert_type solver xv float_type;
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Eq (Type_var x) Float)) (Not (Eq (Type_var x) Int))
          (Eq Int Float))))
       (Asserted (Eq (Type_var x) Float)) (Asserted (Eq (Type_var x) Int))
       (Asserted (Not (Eq Int Float))))))
    |}];
  Solver.pop solver;
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments ((x ((type_ ((Base Int))) (numeric ()) (euf_repr ()))))))
    |}]
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
  assert_ok solver (type_eq (Var a) ft_int);
  assert_ok solver (type_eq (Var a) ft_float);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Unsat
     (core
      ((Theory_lemma
        (Or
         ((Not (Eq (Type_var a) Float)) (Not (Eq (Type_var a) Int))
          (Eq Int Float))))
       (Asserted (Eq (Var a) Float)) (Asserted (Eq (Var a) Int))
       (Asserted (Not (Eq Int Float))))))
    |}]
;;

let%expect_test "Type_eq: TypeEq(a, Int) alone is sat" =
  let a = Tvar.of_string "a" in
  let solver = Solver.create () in
  assert_ok solver (type_eq (Var a) ft_int);
  print_result (Solver.solve solver);
  [%expect
    {|
    (Sat
     (tvar_assignments ((a ((type_ ((Base Int))) (numeric ()) (euf_repr ()))))))
    |}]
;;

let%expect_test "Type_eq: normalize makes Eq(a, b) = Eq(b, a) for embedded \
                 type terms"
  =
  let a = Tvar.of_string "a" in
  let te = Type_expr.Var a in
  let atom1 = Atom.normalize (`Type_eq (te, int_type)) in
  let atom2 = Atom.normalize (`Type_eq (int_type, te)) in
  print_s [%sexp ([%equal: Atom.t] atom1 atom2 : bool)];
  [%expect {| true |}]
;;

let (_ : Formula.any -> Formula.any) = ft_array
let (_ : Type_expr.t) = int_type

(* TODO: test that shows type theory congruence closure doesn't work, then fix
   it. *)
