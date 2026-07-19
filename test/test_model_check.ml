open! Core
open! Feel.Import
open! Theory_core
open! Theory

(* Every satisfiable instance's returned model is independently re-checked with
   [Solver.check_model], which evaluates the asserted formulas under the model's
   atom values and cross-checks the numeric/type witnesses. *)

let x : Formula.any = Var (Tvar.of_string "x")
let y : Formula.any = Var (Tvar.of_string "y")
let z : Formula.any = Var (Tvar.of_string "z")
let f arg : Formula.any = App (Tvar.of_string "f", [ arg ])
let eq a b : Formula.any = Eq (a, b)
let neq a b : Formula.any = Not (eq a b)
let xv = Tvar.of_string "x"
let yv = Tvar.of_string "y"

let assert_ok solver formula =
  match Or_error.ok_exn (Solver.assert_formula solver formula) with
  | `Ok -> ()
  | `Unsat _ -> print_endline "UNSAT (at assert time)"
;;

let check solver =
  match Solver.solve solver with
  | Unsat _ -> print_endline "unexpectedly unsat"
  | Sat { model } ->
    print_s [%sexp (Solver.check_model solver model : unit Or_error.t)]
;;

let%expect_test "boolean model checks" =
  let solver = Solver.create () in
  let a = eq x y in
  let b = eq y z in
  assert_ok solver (Or [ a; b ]);
  assert_ok solver (Or [ Not a; b ]);
  check solver;
  [%expect {| (Ok ()) |}]
;;

let%expect_test "EUF model checks" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  assert_ok solver (eq (f x) z);
  check solver;
  [%expect {| (Ok ()) |}]
;;

let%expect_test "EUF disequality and congruence model checks" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  (* x = y forces f(x) = f(y) by congruence; z is kept distinct. *)
  assert_ok solver (neq (f x) z);
  assert_ok solver (eq (f x) (f y));
  check solver;
  [%expect {| (Ok ()) |}]
;;

let%expect_test "linear-arithmetic model checks" =
  let solver = Solver.create () in
  assert_ok solver (Formula.La_compare (x, `Ge, La_const (Q.of_int 3)));
  assert_ok solver (Formula.La_compare (x, `Le, La_const (Q.of_int 5)));
  assert_ok solver (Formula.La_compare (y, `Lt, x));
  check solver;
  [%expect {| (Ok ()) |}]
;;

let%expect_test "type model checks" =
  let solver = Solver.create () in
  Solver.assert_type solver xv (Base Int);
  Solver.assert_type solver yv (Base Float);
  check solver;
  [%expect {| (Ok ()) |}]
;;

let%expect_test "integer model checks" =
  let solver = Solver.create () in
  Solver.assert_type solver xv (Base Int);
  assert_ok solver (Formula.La_compare (x, `Ge, La_const (Q.of_int 1)));
  assert_ok solver (Formula.La_compare (x, `Le, La_const (Q.of_int 4)));
  check solver;
  [%expect {| (Ok ()) |}]
;;

let%expect_test "Nelson-Oppen model checks (shared var, consistent)" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  assert_ok solver (Formula.La_compare (x, `Ge, La_const (Q.of_int 2)));
  assert_ok solver (Formula.La_compare (y, `Le, La_const (Q.of_int 7)));
  check solver;
  [%expect {| (Ok ()) |}]
;;

(* A deliberately corrupted model must be rejected: flipping a linear atom's
   truth value contradicts the numeric witness. *)
let%expect_test "corrupted model is rejected" =
  let solver = Solver.create () in
  assert_ok solver (Formula.La_compare (x, `Ge, La_const (Q.of_int 3)));
  (match Solver.solve solver with
   | Unsat _ -> print_endline "unexpectedly unsat"
   | Sat { model } ->
     (* Flip only the linear atoms, leaving the base-type axioms intact, so the
        rejection is driven by the [`Ge] assertion becoming false. *)
     let corrupted =
       { model with
         Model.atom_values =
           Map.mapi model.atom_values ~f:(fun ~key ~data ->
             match key with
             | `Le _ -> not data
             | `Eq _ | `Type_eq _ -> data)
       }
     in
     print_s [%sexp (Solver.check_model solver corrupted : unit Or_error.t)]);
  [%expect
    {|
    (Error
     ("asserted formula is false under the model"
      (formula (La_compare (Var x) Ge (La_const ((num 3) (den 1)))))))
    |}]
;;

(* Corrupting the numeric witness (so an atom's truth value no longer matches
   the value it constrains) is caught by the per-atom numeric consistency check,
   even though the boolean structure still evaluates true. *)
let%expect_test "numeric witness inconsistent with atom value is rejected" =
  let solver = Solver.create () in
  assert_ok solver (Formula.La_compare (x, `Ge, La_const (Q.of_int 3)));
  (match Solver.solve solver with
   | Unsat _ -> print_endline "unexpectedly unsat"
   | Sat { model } ->
     let corrupted =
       { model with
         Model.tvar_assignments =
           Map.map model.tvar_assignments ~f:(fun a ->
             { a with numeric = Some (Simplex.Q_eps.of_q (Q.of_int 0)) })
       }
     in
     print_s [%sexp (Solver.check_model solver corrupted : unit Or_error.t)]);
  [%expect
    {|
    (Error
     ("linear atom value disagrees with its model truth value"
      (expression ((coeffs ((x ((num -1) (den 1))))) (const ((num 0) (den 1)))))
      (bound ((num -3) (den 1))) (expected true) (holds false)))
    |}]
;;

(* Splitting the EUF classes (so a true equality's sides land in different
   classes) is caught by the per-atom EUF check. *)
let%expect_test "EUF class map inconsistent with a true equality is rejected" =
  let solver = Solver.create () in
  assert_ok solver (eq x y);
  (match Solver.solve solver with
   | Unsat _ -> print_endline "unexpectedly unsat"
   | Sat { model } ->
     (* Make every term its own representative, breaking x = y. *)
     let corrupted =
       { model with
         Model.euf_classes =
           Map.mapi model.euf_classes ~f:(fun ~key ~data:_ -> key)
       }
     in
     print_s [%sexp (Solver.check_model solver corrupted : unit Or_error.t)]);
  [%expect
    {|
    (Error
     ("EUF equality value disagrees with the equivalence classes" (a (Var x))
      (b (Var y)) (expected true) (classes_agree false)))
    |}]
;;

(* A satisfiable type disequality between differently-typed variables checks;
   corrupting a witness so the two share a type makes the checker reject it. *)
let%expect_test "type disequality checks; corrupted type witness is rejected" =
  let solver = Solver.create () in
  Solver.assert_type solver xv (Base Int);
  Solver.assert_type solver yv (Base Float);
  assert_ok solver (neq (Type_var xv) (Type_var yv));
  (match Solver.solve solver with
   | Unsat _ -> print_endline "unexpectedly unsat"
   | Sat { model } ->
     print_s [%sexp (Solver.check_model solver model : unit Or_error.t)];
     (* Force y's type to Int too: now the false type equality x <> y is
        contradicted by both being Int. *)
     let corrupted =
       { model with
         Model.tvar_assignments =
           Map.update model.tvar_assignments yv ~f:(function
             | None -> assert false
             | Some a -> { a with type_ = Some (Base Int) })
       }
     in
     print_s [%sexp (Solver.check_model solver corrupted : unit Or_error.t)]);
  [%expect
    {|
    (Ok ())
    (Error
     ("type equality value disagrees with the assigned ground types"
      (ta (Base Int)) (tb (Base Int)) (expected false) (types_equal true)))
    |}]
;;
