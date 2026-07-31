open! Core
open! Feel.Import
open! Theory_core
open! Theory

let datatype = Datatype.Datatype.{ name = Tvar.of_string "list" }

let nil_constructor =
  Datatype.Constructor.{ datatype; name = Tvar.of_string "Nil"; arity = 0 }
;;

let cons_constructor =
  Datatype.Constructor.{ datatype; name = Tvar.of_string "Cons"; arity = 2 }
;;

let head_selector =
  Datatype.Selector.
    { constructor = cons_constructor; name = Tvar.of_string "head"; index = 0 }
;;

let tail_selector =
  Datatype.Selector.
    { constructor = cons_constructor; name = Tvar.of_string "tail"; index = 1 }
;;

let v name : Formula.any = Var (Tvar.of_string name)
let nil : Formula.any = Datatype_constructor (nil_constructor, [])

let cons head tail : Formula.any =
  Datatype_constructor (cons_constructor, [ head; tail ])
;;

let is_nil value : Formula.any = Datatype_tester (nil_constructor, value)
let is_cons value : Formula.any = Datatype_tester (cons_constructor, value)
let head value : Formula.any = Datatype_selector (head_selector, value)
let tail value : Formula.any = Datatype_selector (tail_selector, value)
let eq left right : Formula.any = Eq (left, right)
let neq left right : Formula.any = Not (eq left right)

let assert_ok solver formula =
  match Or_error.ok_exn (Solver.assert_formula solver formula) with
  | `Ok -> ()
  | `Unsat _ -> print_endline "Unsat at assert time"
;;

let print_result = function
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

let%expect_test "constructor injectivity" =
  let solver = Solver.create () in
  let a = v "a" in
  let b = v "b" in
  let c = v "c" in
  let d = v "d" in
  assert_ok solver (eq (cons a b) (cons c d));
  assert_ok solver (neq a c);
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "constructor disjointness" =
  let solver = Solver.create () in
  assert_ok solver (eq nil (cons (v "h") (v "t")));
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "testers on matching and non-matching constructors" =
  let solver = Solver.create () in
  assert_ok solver (is_nil nil);
  assert_ok solver (Not (is_cons nil));
  assert_ok solver (is_cons (cons (v "h") (v "t")));
  assert_ok solver (Not (is_nil (cons (v "h") (v "t"))));
  print_result (Solver.solve solver);
  [%expect {| Sat |}]
;;

let%expect_test "tester contradiction" =
  let solver = Solver.create () in
  assert_ok solver (is_nil (cons (v "h") (v "t")));
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "selector projection on matching constructor" =
  let solver = Solver.create () in
  let h = v "h" in
  let t = v "t" in
  assert_ok solver (neq (head (cons h t)) h);
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "selector on wrong constructor is underspecified" =
  let solver = Solver.create () in
  assert_ok solver (neq (head nil) (v "anything"));
  print_result (Solver.solve solver);
  [%expect {| Sat |}]
;;

let%expect_test "direct acyclicity" =
  let solver = Solver.create () in
  let x = v "x" in
  assert_ok solver (eq x (cons x nil));
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "multi-step acyclicity" =
  let solver = Solver.create () in
  let x = v "x" in
  let y = v "y" in
  assert_ok solver (eq x (cons y nil));
  assert_ok solver (eq y (cons x nil));
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "satisfiable ADT problem has a checkable model" =
  let solver = Solver.create () in
  let h = v "h" in
  let t = v "t" in
  assert_ok solver (is_cons (cons h t));
  assert_ok solver (eq (head (cons h t)) h);
  match Solver.solve solver with
  | Unsat _ -> print_endline "Unsat"
  | Sat { model } ->
    print_endline "Sat";
    print_s [%sexp (Solver.check_model solver model : unit Or_error.t)];
    [%expect {|
    Sat
    (Ok ())
    |}]
;;

let%expect_test "ADT proof certificates" =
  let cases =
    [ ( "injectivity"
      , [ eq (cons (v "a") nil) (cons (v "c") nil); neq (v "a") (v "c") ] )
    ; "disjointness", [ eq nil (cons (v "h") (v "t")) ]
    ; "selector", [ neq (head (cons (v "h") nil)) (v "h") ]
    ; "tester", [ is_nil (cons (v "h") nil) ]
    ; "acyclicity", [ eq (v "x") (cons (v "x") nil) ]
    ]
  in
  List.iter cases ~f:(fun (name, formulas) ->
    let solver = Solver.create ~config:{ produce_proofs = true } () in
    List.iter formulas ~f:(assert_ok solver);
    print_string name;
    print_string ": ";
    print_proof_result (Solver.solve solver));
  [%expect
    {|
    injectivity: (Unsat (proof_check (Ok ())))
    disjointness: (Unsat (proof_check (Ok ())))
    selector: (Unsat (proof_check (Ok ())))
    tester: (Unsat (proof_check (Ok ())))
    acyclicity: (Unsat (proof_check (Ok ())))
    |}]
;;

let%expect_test "ADT payloads interact with linear arithmetic" =
  let solver = Solver.create () in
  let a = v "a" in
  let c = v "c" in
  assert_ok solver (eq (cons a nil) (cons c nil));
  assert_ok solver (La_compare (a, `Lt, c));
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;
