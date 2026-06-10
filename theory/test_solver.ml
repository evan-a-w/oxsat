open! Core
open! Feel.Import

let x = `Var (Tvar.of_string "x")
let y = `Var (Tvar.of_string "y")
let z = `Var (Tvar.of_string "z")
let f arg : Term.t = `App (~function_:(Tvar.of_string "f"), ~args:[ arg ])
let eq a b : Formula.t = Atom (`Eq (a, b))
let neq a b : Formula.t = Not (eq a b)

let print_result (result : Feel.Sat_result.t) =
  match result with
  | Sat _ -> print_endline "SAT"
  | Unsat _ -> print_endline "UNSAT"
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

let sexp_of_atom (`Eq (a, b) : Atom.t) : Sexp.t =
  List [ Atom "Eq"; sexp_of_term a; sexp_of_term b ]
;;

let%expect_test "Atom.normalize orders Eq sides canonically" =
  let a = `Eq (x, y) in
  let b = `Eq (y, x) in
  print_s [%sexp (sexp_of_atom (Atom.normalize a) : Sexp.t)];
  print_s [%sexp (sexp_of_atom (Atom.normalize b) : Sexp.t)];
  [%expect
    {|
    (Eq (Var x) (Var y))
    (Eq (Var x) (Var y))
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
  let formula : Formula.t =
    And [ Atom a; Or [ Atom b; Not (Atom a) ] ]
  in
  let encoding = Formula.Encoding.create () in
  let clauses = Formula.encode encoding formula in
  let solver = Feel.Solver.create () in
  List.iter clauses ~f:(fun clause ->
    ignore (Feel.Solver.add_clause solver ~clause : [ `Ok | `Unsat of int array ]));
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

let%expect_test "Tseitin: atom -> sat_var mapping is stable across encode calls" =
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
    List.iter
      (Formula.encode encoding formula)
      ~f:(fun clause ->
        ignore (Feel.Solver.add_clause solver ~clause : [ `Ok | `Unsat of int array ]))
  in
  assert_formula True;
  assert_formula (Not (Not True));
  print_result (Feel.Solver.solve solver);
  [%expect {| SAT |}]
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
    List.iter
      (Formula.encode encoding formula)
      ~f:(fun clause ->
        ignore (Feel.Solver.add_clause solver ~clause : [ `Ok | `Unsat of int array ]))
  in
  assert_formula (And []);
  assert_formula (Not (Or []));
  print_result (Feel.Solver.solve solver);
  [%expect {| SAT |}]
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
  (* (a \/ b) /\ (~a \/ b) /\ (a \/ ~b)  =>  b must be true *)
  assert_ok solver (Or [ Atom a; Atom b ]);
  assert_ok solver (Or [ Not (Atom a); Atom b ]);
  assert_ok solver (Or [ Atom a; Not (Atom b) ]);
  print_result (Solver.solve solver);
  [%expect {| SAT |}]
;;

let%expect_test "EUF: transitivity violation is unsat" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  assert_ok solver (neq y z);
  assert_ok solver (eq x z);
  print_result (Solver.solve solver);
  [%expect {| UNSAT |}]
;;

let%expect_test "EUF: congruence conflict (f(x) <> f(y) with x = y)" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  assert_ok solver (neq (f x) (f y));
  print_result (Solver.solve solver);
  [%expect {| UNSAT |}]
;;

let%expect_test "EUF: congruence positive propagation (x = y forces f(x) = f(y))" =
  let solver = Solver.create () in
  (* assert f(x) <> f(y) first, then x = y -- should still detect conflict *)
  assert_ok solver (neq (f x) (f y));
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect {| UNSAT |}]
;;

let%expect_test "EUF: satisfiable case" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  assert_ok solver (neq y z);
  print_result (Solver.solve solver);
  [%expect {| SAT |}]
;;

(* ----- Incremental assert_formula + solve ----- *)

(* Asserting a unit clause that directly contradicts an already-true literal
   at decision level 0 is rejected by [assert_formula] itself (mirroring
   [Feel.Solver.add_clause]'s reject-on-conflict semantics): the conflicting
   clause is never added, so the solver's state is unchanged and the next
   [solve] still reports the prior (now-stale) result. *)
let%expect_test "incremental: asserting a contradicting formula after solve" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect {| SAT |}];
  assert_ok solver (neq x y);
  print_result (Solver.solve solver);
  [%expect {|
    UNSAT (at assert time)
    SAT
    |}]
;;

let%expect_test "incremental: new EUF atoms registered after a solve" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect {| SAT |}];
  (* introduce brand-new terms/atoms involving f, after solving once *)
  assert_ok solver (neq (f x) (f y));
  print_result (Solver.solve solver);
  [%expect {| UNSAT |}]
;;

(* ----- Push/pop ----- *)

let%expect_test "push/pop: retracting a contradicting constraint" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect {| SAT |}];
  Solver.push solver;
  assert_ok solver (neq x y);
  print_result (Solver.solve solver);
  [%expect {| UNSAT |}];
  Solver.pop solver;
  print_result (Solver.solve solver);
  [%expect {| SAT |}]
;;

let%expect_test "push/pop: nested scopes" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect {| SAT |}];
  Solver.push solver;
  assert_ok solver (neq y z);
  print_result (Solver.solve solver);
  [%expect {| SAT |}];
  Solver.push solver;
  assert_ok solver (eq x z);
  (* x=y, y<>z, x=z is a transitivity violation *)
  print_result (Solver.solve solver);
  [%expect {| UNSAT |}];
  Solver.pop solver;
  (* back to: x=y, y<>z -- satisfiable *)
  print_result (Solver.solve solver);
  [%expect {| SAT |}];
  Solver.pop solver;
  (* back to: x=y only -- satisfiable *)
  print_result (Solver.solve solver);
  [%expect {| SAT |}]
;;

let%expect_test "push/pop: EUF congruence conflict inside a scope is retracted on pop" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  print_result (Solver.solve solver);
  [%expect {| SAT |}];
  Solver.push solver;
  assert_ok solver (neq (f x) (f y));
  print_result (Solver.solve solver);
  [%expect {| UNSAT |}];
  Solver.pop solver;
  print_result (Solver.solve solver);
  [%expect {| SAT |}];
  (* solving again should remain consistent *)
  print_result (Solver.solve solver);
  [%expect {| SAT |}]
;;

(* ----- Stats passthrough ----- *)

let%expect_test "stats passthrough" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  ignore (Solver.solve solver : Feel.Sat_result.t);
  let stats = Solver.stats solver in
  print_s [%message "" ~iterations_nonneg:(stats.#iterations >= 0 : bool)];
  [%expect {| (iterations_nonneg true) |}]
;;
