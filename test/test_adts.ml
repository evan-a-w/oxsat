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

let theory_literal atom ~positive =
  Proof.Literal.create ~atom:(Proof.Atom.Theory atom) ~positive
;;

let clause_exn literals =
  match Proof.Clause.create literals with
  | `Clause clause -> clause
  | `Tautology -> failwith "unexpected tautology"
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

let%expect_test "scoped ADT selector premise can be popped" =
  let solver = Solver.create () in
  let x = v "x" in
  let h = v "h" in
  assert_ok solver (neq (head x) h);
  Solver.push solver;
  assert_ok solver (eq x (cons h nil));
  print_result (Solver.solve solver);
  Solver.pop solver;
  print_result (Solver.solve solver);
  [%expect {|
    Unsat
    Sat
    |}]
;;

let%expect_test "scoped ADT tester premise can be popped" =
  let solver = Solver.create () in
  let x = v "x" in
  let h = v "h" in
  assert_ok solver (is_nil x);
  Solver.push solver;
  assert_ok solver (eq x (cons h nil));
  print_result (Solver.solve solver);
  Solver.pop solver;
  print_result (Solver.solve solver);
  [%expect {|
    Unsat
    Sat
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

let%expect_test "ADT proofs print human-readable certificate text" =
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
    match Solver.solve solver with
    | Sat _ -> print_endline (name ^ ": sat")
    | Unsat { proof = Some proof; _ } ->
      print_endline
        (sprintf "%s: check = %b" name (Or_error.is_ok (Proof.check proof)));
      print_endline (Proof.to_string_hum proof)
    | Unsat { proof = None; _ } -> print_endline (name ^ ": no proof"));
  [%expect
    {|
    injectivity: check = true
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: Cons(a, Nil()) = Cons(c, Nil())
      a4: a ≠ c
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: Cons(a, Nil()) = Cons(c, Nil())   [assumption a3]
      s4: a ≠ c   [assumption a4]
      s5: false   [refutation of [s0, s1, s2, s3, s4]]
        refutation:
          steps:
            r0: Cons(a, Nil()) = Cons(c, Nil())   [s3]
            r1: a ≠ c   [s4]
            r2: a = c ∨ Cons(a, Nil()) ≠ Cons(c, Nil())   [ADT: (Injectivity (constructor ((datatype ((name list))) (name Cons) (arity 2)))
     (left_args
      ((Var a)
       (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0)) ())))
     (right_args
      ((Var c)
       (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0)) ())))
     (field_index 0))]
            r3: ⊥   [RUP over [r0, r1, r2]]
    Conclusion: s5

    disjointness: check = true
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: Nil() = Cons(h, t)
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: Nil() = Cons(h, t)   [assumption a3]
      s4: false   [refutation of [s0, s1, s2, s3]]
        refutation:
          steps:
            r0: Nil() = Cons(h, t)   [s3]
            r1: Nil() ≠ Cons(h, t)   [ADT: (Disjointness
     (left_constructor ((datatype ((name list))) (name Cons) (arity 2)))
     (left_args ((Var h) (Var t)))
     (right_constructor ((datatype ((name list))) (name Nil) (arity 0)))
     (right_args ()))]
            r2: ⊥   [RUP over [r0, r1]]
    Conclusion: s4

    selector: check = true
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: head(Cons(h, Nil())) ≠ h
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: head(Cons(h, Nil())) ≠ h   [assumption a3]
      s4: false   [refutation of [s0, s1, s2, s3]]
        refutation:
          steps:
            r0: h ≠ head(Cons(h, Nil()))   [s3]
            r1: h = head(Cons(h, Nil()))   [ADT: (Selector
     (selector
      ((constructor ((datatype ((name list))) (name Cons) (arity 2))) (name head)
       (index 0)))
     (argument
      (Datatype_constructor ((datatype ((name list))) (name Cons) (arity 2))
       ((Var h)
        (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0)) ()))))
     (constructor_args
      ((Var h)
       (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0)) ()))))]
            r2: ⊥   [RUP over [r0, r1]]
    Conclusion: s4

    tester: check = true
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: is-Nil(Cons(h, Nil()))
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: is-Nil(Cons(h, Nil()))   [assumption a3]
      s4: false   [refutation of [s0, s1, s2, s3]]
        refutation:
          steps:
            r0: true = is-Nil(Cons(h, Nil()))   [s3]
            r1: true ≠ is-Nil(Cons(h, Nil()))   [ADT: (Tester (tester_constructor ((datatype ((name list))) (name Nil) (arity 0)))
     (argument
      (Datatype_constructor ((datatype ((name list))) (name Cons) (arity 2))
       ((Var h)
        (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0)) ()))))
     (witness_constructor ((datatype ((name list))) (name Cons) (arity 2)))
     (witness_args
      ((Var h)
       (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0)) ())))
     (value false))]
            r2: ⊥   [RUP over [r0, r1]]
    Conclusion: s4

    acyclicity: check = true
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: x = Cons(x, Nil())
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: x = Cons(x, Nil())   [assumption a3]
      s4: false   [refutation of [s0, s1, s2, s3]]
        refutation:
          steps:
            r0: x = Cons(x, Nil())   [s3]
            r1: x ≠ Cons(x, Nil())   [ADT: (Acyclicity
     (cycle
      (((constructor_term
         (Datatype_constructor ((datatype ((name list))) (name Cons) (arity 2))
          ((Var x)
           (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0))
            ()))))
        (field (Var x))))))]
            r2: ⊥   [RUP over [r0, r1]]
    Conclusion: s4
    |}]
;;

let%expect_test "bogus ADT certificates are rejected" =
  let arg = v "arg" in
  let h = v "h" in
  let t = v "t" in
  let a = v "a" in
  let c = v "c" in
  let check clause certificate =
    Proof.check_theory_certificate ~clause (Adt certificate)
  in
  let disjointness_clause =
    clause_exn
      [ theory_literal
          (`Eq
            ( Formula.Datatype_constructor (nil_constructor, [])
            , Formula.Datatype_constructor (nil_constructor, []) ))
          ~positive:false
      ]
  in
  let selector_clause =
    clause_exn
      [ theory_literal
          (`Eq (Formula.Datatype_selector (head_selector, arg), t))
          ~positive:true
      ]
  in
  let acyclicity_clause =
    clause_exn
      [ theory_literal (`Eq (v "x", v "y")) ~positive:false
      ; theory_literal (`Eq (v "y", v "x")) ~positive:false
      ]
  in
  let injectivity_clause =
    clause_exn
      [ theory_literal
          (`Eq
            ( Formula.Datatype_constructor (cons_constructor, [ a; nil ])
            , Formula.Datatype_constructor (cons_constructor, [ c; nil ]) ))
          ~positive:false
      ; theory_literal (`Eq (a, c)) ~positive:true
      ]
  in
  print_s
    [%message
      "bogus ADT certificates"
        ~disjointness_same_constructor:
          (Or_error.is_error
             (check
                disjointness_clause
                (Disjointness
                   { left_constructor = nil_constructor
                   ; left_args = []
                   ; right_constructor = nil_constructor
                   ; right_args = []
                   }))
           : bool)
        ~selector_wrong_field:
          (Or_error.is_error
             (check
                selector_clause
                (Selector
                   { selector = head_selector
                   ; argument = arg
                   ; constructor_args = [ h; t ]
                   }))
           : bool)
        ~acyclicity_not_a_constructor:
          (Or_error.is_error
             (check
                acyclicity_clause
                (Acyclicity
                   { cycle =
                       [ { constructor_term = v "not_a_constructor"
                         ; field = v "x"
                         }
                       ]
                   }))
           : bool)
        ~acyclicity_field_not_in_args:
          (Or_error.is_error
             (check
                acyclicity_clause
                (Acyclicity
                   { cycle =
                       [ { constructor_term = cons h nil; field = v "nope" } ]
                   }))
           : bool)
        ~injectivity_field_out_of_bounds:
          (Or_error.is_error
             (check
                injectivity_clause
                (Injectivity
                   { constructor = cons_constructor
                   ; left_args = [ a; nil ]
                   ; right_args = [ c; nil ]
                   ; field_index = 3
                   }))
           : bool)];
  print_s
    [%sexp
      (check
         disjointness_clause
         (Disjointness
            { left_constructor = nil_constructor
            ; left_args = []
            ; right_constructor = nil_constructor
            ; right_args = []
            })
       : unit Or_error.t)];
  [%expect
    {|
    ("bogus ADT certificates" (disjointness_same_constructor true)
     (selector_wrong_field true) (acyclicity_not_a_constructor true)
     (acyclicity_field_not_in_args true) (injectivity_field_out_of_bounds true))
    (Error "ADT certificate does not match its clause")
    |}]
;;
