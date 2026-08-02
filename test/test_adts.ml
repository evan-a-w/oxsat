open! Core
open! Feel.Import
open! Theory_core
open! Theory

let datatype = Datatype.Datatype.{ name = Tvar.of_string "list" }
let list_type = Type_expr.App (datatype.name, [])

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

let list_declaration =
  Datatype.Declaration.
    { datatype
    ; constructors =
        [ { constructor = nil_constructor; field_types = []; selectors = [] }
        ; { constructor = cons_constructor
          ; field_types = [ list_type; list_type ]
          ; selectors = [ head_selector; tail_selector ]
          }
        ]
    }
;;

let datatype_env =
  Or_error.ok_exn (Datatype.Env.of_declarations [ list_declaration ])
;;

let create_solver ?(produce_proofs = false) () =
  Solver.create ~config:{ produce_proofs; datatype_env } ()
;;

let color_datatype = Datatype.Datatype.{ name = Tvar.of_string "color" }

let red_constructor =
  Datatype.Constructor.
    { datatype = color_datatype; name = Tvar.of_string "Red"; arity = 0 }
;;

let green_constructor =
  Datatype.Constructor.
    { datatype = color_datatype; name = Tvar.of_string "Green"; arity = 0 }
;;

let blue_constructor =
  Datatype.Constructor.
    { datatype = color_datatype; name = Tvar.of_string "Blue"; arity = 0 }
;;

let color_declaration =
  Datatype.Declaration.
    { datatype = color_datatype
    ; constructors =
        List.map
          [ red_constructor; green_constructor; blue_constructor ]
          ~f:(fun constructor ->
            { Datatype.Constructor_declaration.constructor
            ; field_types = []
            ; selectors = []
            })
    }
;;

let color_env =
  Or_error.ok_exn (Datatype.Env.of_declarations [ color_declaration ])
;;

let create_color_solver ?(produce_proofs = false) () =
  Solver.create ~config:{ produce_proofs; datatype_env = color_env } ()
;;

let v name : Formula.any = Var (Tvar.of_string name)
let red : Formula.any = Datatype_constructor (red_constructor, [])
let green : Formula.any = Datatype_constructor (green_constructor, [])
let blue : Formula.any = Datatype_constructor (blue_constructor, [])
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
  let solver = create_solver () in
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
  let solver = create_solver () in
  assert_ok solver (eq nil (cons (v "h") (v "t")));
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "testers on matching and non-matching constructors" =
  let solver = create_solver () in
  assert_ok solver (is_nil nil);
  assert_ok solver (Not (is_cons nil));
  assert_ok solver (is_cons (cons (v "h") (v "t")));
  assert_ok solver (Not (is_nil (cons (v "h") (v "t"))));
  print_result (Solver.solve solver);
  [%expect {| Sat |}]
;;

let%expect_test "tester contradiction" =
  let solver = create_solver () in
  assert_ok solver (is_nil (cons (v "h") (v "t")));
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "selector projection on matching constructor" =
  let solver = create_solver () in
  let h = v "h" in
  let t = v "t" in
  assert_ok solver (neq (head (cons h t)) h);
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "selector on wrong constructor is underspecified" =
  let solver = create_solver () in
  assert_ok solver (neq (head nil) (v "anything"));
  print_result (Solver.solve solver);
  [%expect {| Sat |}]
;;

let%expect_test "undeclared ADT terms are rejected" =
  let solver = Solver.create () in
  print_s [%sexp (Solver.assert_formula solver (eq red red) : _ Or_error.t)];
  [%expect
    {|
    (Error
     ("undeclared ADT constructor"
      (constructor ((datatype ((name color))) (name Red) (arity 0)))))
    |}]
;;

let%expect_test "wrong-arity ADT constructor applications are rejected" =
  let solver = create_solver () in
  let wrong_nil : Formula.any =
    Datatype_constructor (nil_constructor, [ v "x" ])
  in
  let wrong_cons : Formula.any =
    Datatype_constructor (cons_constructor, [ nil ])
  in
  print_s
    [%sexp
      (Solver.assert_formula solver (eq wrong_nil wrong_nil) : _ Or_error.t)];
  print_s
    [%sexp
      (Solver.assert_formula solver (eq wrong_cons wrong_cons) : _ Or_error.t)];
  [%expect
    {|
    (Error
     ("ADT constructor application has the wrong arity"
      (constructor ((datatype ((name list))) (name Nil) (arity 0))) (actual 1)))
    (Error
     ("ADT constructor application has the wrong arity"
      (constructor ((datatype ((name list))) (name Cons) (arity 2))) (actual 1)))
    |}]
;;

let%expect_test "enum exhaustiveness" =
  let solver = create_color_solver () in
  let x = v "x" in
  assert_ok solver (neq x red);
  print_result (Solver.solve solver);
  assert_ok solver (neq x green);
  assert_ok solver (neq x blue);
  print_result (Solver.solve solver);
  [%expect {|
    Sat
    Unsat
    |}]
;;

let%expect_test "scoped enum exhaustiveness can be popped" =
  let solver = create_color_solver () in
  let x = v "x" in
  assert_ok solver (neq x red);
  assert_ok solver (neq x green);
  Solver.push solver;
  assert_ok solver (neq x blue);
  print_result (Solver.solve solver);
  Solver.pop solver;
  print_result (Solver.solve solver);
  [%expect {|
    Unsat
    Sat
    |}]
;;

let%expect_test "non-enum constructor completeness" =
  let solver = create_solver () in
  let x = v "x" in
  assert_ok solver (Not (is_nil x));
  assert_ok solver (Not (is_cons x));
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "scoped datatype declarations can be popped" =
  let solver = Solver.create () in
  Solver.push solver;
  print_s
    [%sexp (Solver.declare_datatype solver color_declaration : unit Or_error.t)];
  print_s [%sexp (Solver.assert_formula solver (eq red red) : _ Or_error.t)];
  assert_ok solver (neq (v "x") red);
  (match Solver.solve solver with
   | Sat _ -> print_endline "scoped solve sat"
   | Unsat { core; _ } ->
     print_s [%sexp (core : Solver_result.Core_step.t list)]);
  Solver.pop solver;
  print_s [%sexp (Solver.assert_formula solver (eq red red) : _ Or_error.t)];
  [%expect
    {|
    (Ok ())
    (Ok _)
    scoped solve sat
    (Error
     ("undeclared ADT constructor"
      (constructor ((datatype ((name color))) (name Red) (arity 0)))))
    |}]
;;

let%expect_test "ADT declarations are validated" =
  let bad_selector =
    Datatype.Selector.
      { constructor = nil_constructor; name = Tvar.of_string "bad"; index = 0 }
  in
  let duplicate_constructor_name =
    Datatype.Constructor.
      { datatype; name = nil_constructor.name; arity = cons_constructor.arity }
  in
  let cases =
    [ ("empty", Datatype.Declaration.{ datatype; constructors = [] })
    ; ( "wrong selector index"
      , { datatype
        ; constructors =
            [ { constructor = nil_constructor
              ; field_types = []
              ; selectors = [ bad_selector ]
              }
            ]
        } )
    ; ( "duplicate constructor name"
      , { datatype
        ; constructors =
            [ { constructor = nil_constructor
              ; field_types = []
              ; selectors = []
              }
            ; { constructor = duplicate_constructor_name
              ; field_types = [ list_type; list_type ]
              ; selectors = []
              }
            ]
        } )
    ; ( "wrong field type arity"
      , { datatype
        ; constructors =
            [ { constructor = cons_constructor
              ; field_types = [ list_type ]
              ; selectors = []
              }
            ]
        } )
    ; ( "no finite inhabitant"
      , { datatype
        ; constructors =
            [ { constructor = cons_constructor
              ; field_types = [ list_type; list_type ]
              ; selectors = []
              }
            ]
        } )
    ]
  in
  List.iter cases ~f:(fun (name, declaration) ->
    print_s
      [%message
        (name : string)
          ~is_error:
            (Or_error.is_error (Datatype.Env.of_declarations [ declaration ])
             : bool)]);
  [%expect
    {|
    ((name empty) (is_error true))
    ((name "wrong selector index") (is_error true))
    ((name "duplicate constructor name") (is_error true))
    ((name "wrong field type arity") (is_error true))
    ((name "no finite inhabitant") (is_error true))
    |}]
;;

let%expect_test "ADT field types constrain constructor arguments" =
  let box_datatype = Datatype.Datatype.{ name = Tvar.of_string "box" } in
  let box_constructor =
    Datatype.Constructor.
      { datatype = box_datatype; name = Tvar.of_string "Box"; arity = 1 }
  in
  let box_selector =
    Datatype.Selector.
      { constructor = box_constructor
      ; name = Tvar.of_string "unbox"
      ; index = 0
      }
  in
  let box_declaration =
    Datatype.Declaration.
      { datatype = box_datatype
      ; constructors =
          [ { constructor = box_constructor
            ; field_types = [ Type_expr.Base Int ]
            ; selectors = [ box_selector ]
            }
          ]
      }
  in
  let datatype_env =
    Or_error.ok_exn (Datatype.Env.of_declarations [ box_declaration ])
  in
  let solver =
    Solver.create ~config:{ Solver.Config.default with datatype_env } ()
  in
  let x = Tvar.of_string "x" in
  let box_x : Formula.any = Datatype_constructor (box_constructor, [ Var x ]) in
  assert_ok solver (eq box_x box_x);
  print_result (Solver.solve solver);
  print_s [%sexp (Solver.get_type solver x : Type_expr.t option)];
  let solver =
    Solver.create ~config:{ Solver.Config.default with datatype_env } ()
  in
  let b = Tvar.of_string "b" in
  let y = Tvar.of_string "y" in
  let unbox_b : Formula.any = Datatype_selector (box_selector, Var b) in
  assert_ok solver (eq unbox_b (Var y));
  print_result (Solver.solve solver);
  print_s
    [%sexp
      (Solver.get_type solver b : Type_expr.t option)
      , (Solver.get_type solver y : Type_expr.t option)];
  let solver =
    Solver.create ~config:{ Solver.Config.default with datatype_env } ()
  in
  Solver.assert_type solver x (Base Float);
  assert_ok solver (eq box_x box_x);
  print_result (Solver.solve solver);
  [%expect
    {|
    Sat
    ((Base Int))
    Sat
    (((App box ())) ((Base Int)))
    Unsat
    |}]
;;

let%expect_test "non-ground ADT testers are mutually exclusive" =
  let solver = create_solver () in
  assert_ok solver (is_nil (v "x"));
  assert_ok solver (is_cons (v "x"));
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "non-ground ADT model chooses an unobserved constructor" =
  let tree = Datatype.Datatype.{ name = Tvar.of_string "tree" } in
  let tree_type = Type_expr.App (tree.name, []) in
  let leaf =
    Datatype.Constructor.
      { datatype = tree; name = Tvar.of_string "Leaf"; arity = 0 }
  in
  let node =
    Datatype.Constructor.
      { datatype = tree; name = Tvar.of_string "Node"; arity = 2 }
  in
  let declaration =
    Datatype.Declaration.
      { datatype = tree
      ; constructors =
          [ { constructor = leaf; field_types = []; selectors = [] }
          ; { constructor = node
            ; field_types = [ tree_type; tree_type ]
            ; selectors = []
            }
          ]
      }
  in
  let datatype_env =
    Or_error.ok_exn (Datatype.Env.of_declarations [ declaration ])
  in
  let solver =
    Solver.create ~config:{ Solver.Config.default with datatype_env } ()
  in
  let x = v "x" in
  assert_ok solver (Not (Formula.Datatype_tester (leaf, x)));
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

let%expect_test "non-ground tester reconstructs constructor value" =
  let solver = create_solver () in
  let x = v "x" in
  assert_ok solver (is_cons x);
  assert_ok solver (neq x (cons (head x) (tail x)));
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "ADT selector alias projects through variable equal to \
                 constructor"
  =
  let solver = create_solver () in
  let x = v "x" in
  let h = v "h" in
  assert_ok solver (eq x (cons h nil));
  assert_ok solver (neq (head x) h);
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "ADT tester alias follows variable equal to constructor" =
  let solver = create_solver () in
  let x = v "x" in
  let h = v "h" in
  assert_ok solver (eq x (cons h nil));
  assert_ok solver (is_nil x);
  print_result (Solver.solve solver);
  let solver = create_solver () in
  let x = v "x" in
  let h = v "h" in
  assert_ok solver (eq x (cons h nil));
  assert_ok solver (Not (is_cons x));
  print_result (Solver.solve solver);
  [%expect {|
    Unsat
    Unsat
    |}]
;;

let%expect_test "direct acyclicity" =
  let solver = create_solver () in
  let x = v "x" in
  assert_ok solver (eq x (cons x nil));
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "multi-step acyclicity" =
  let solver = create_solver () in
  let x = v "x" in
  let y = v "y" in
  assert_ok solver (eq x (cons y nil));
  assert_ok solver (eq y (cons x nil));
  print_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "satisfiable ADT problem has a checkable model" =
  let solver = create_solver () in
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
  let solver = create_solver () in
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
  let solver = create_solver () in
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

let%expect_test "guarded ADT completeness proof certificate is accepted" =
  let x = v "x" in
  let guard : Atom.Equality.t = `Eq (v "guard_l", v "guard_r") in
  let clause =
    clause_exn
      [ theory_literal (guard :> Atom.t) ~positive:false
      ; theory_literal (`Eq (x, red)) ~positive:true
      ; theory_literal (`Eq (x, green)) ~positive:true
      ; theory_literal (`Eq (x, blue)) ~positive:true
      ]
  in
  print_s
    [%sexp
      (Proof.check_theory_certificate
         ~datatype_env:color_env
         ~clause
         (Adt
            (Completeness
               { declaration = color_declaration
               ; subject = x
               ; guard = Some guard
               ; form = Enum_equalities
               }))
       : unit Or_error.t)];
  [%expect {| (Ok ()) |}]
;;

let%expect_test "ADT proof for match-shaped Boolean ite" =
  let solver = create_solver ~produce_proofs:true () in
  let x = v "x" in
  let result = cons x nil in
  assert_ok solver (Not (Ite (is_cons result, eq (head result) x, False)));
  print_proof_result (Solver.solve solver);
  [%expect {| (Unsat (proof_check (Ok ()))) |}]
;;

let%expect_test "ADT proof certificates" =
  let cases =
    [ ( create_solver
      , "injectivity"
      , [ eq (cons (v "a") nil) (cons (v "c") nil); neq (v "a") (v "c") ] )
    ; create_solver, "disjointness", [ eq nil (cons (v "h") (v "t")) ]
    ; create_solver, "selector", [ neq (head (cons (v "h") nil)) (v "h") ]
    ; create_solver, "tester", [ is_nil (cons (v "h") nil) ]
    ; create_solver, "tester exclusivity", [ is_nil (v "x"); is_cons (v "x") ]
    ; ( create_solver
      , "tester reconstruction"
      , [ is_cons (v "x"); neq (v "x") (cons (head (v "x")) (tail (v "x"))) ] )
    ; create_solver, "acyclicity", [ eq (v "x") (cons (v "x") nil) ]
    ; ( create_solver
      , "constructor completeness"
      , [ Not (is_nil (v "x")); Not (is_cons (v "x")) ] )
    ; ( create_color_solver
      , "enum completeness"
      , [ neq (v "x") red; neq (v "x") green; neq (v "x") blue ] )
    ]
  in
  List.iter cases ~f:(fun (create_solver, name, formulas) ->
    let solver = create_solver ~produce_proofs:true () in
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
    tester exclusivity: (Unsat (proof_check (Ok ())))
    tester reconstruction: (Unsat (proof_check (Ok ())))
    acyclicity: (Unsat (proof_check (Ok ())))
    constructor completeness: (Unsat (proof_check (Ok ())))
    enum completeness: (Unsat (proof_check (Ok ())))
    |}]
;;

let%expect_test "ADT payloads interact with linear arithmetic" =
  let solver = create_solver () in
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
    let solver = create_solver ~produce_proofs:true () in
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
      a3: a : list()
      a4: c : list()
      a5: Cons(a, Nil()) = Cons(c, Nil())
      a6: a ≠ c
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: a : list()   [assumption a3]
      s4: c : list()   [assumption a4]
      s5: Cons(a, Nil()) = Cons(c, Nil())   [assumption a5]
      s6: a ≠ c   [assumption a6]
      s7: false   [refutation of [s0, s1, s2, s3, s4, s5, s6]]
        refutation:
          steps:
            r0: Cons(a, Nil()) = Cons(c, Nil())   [s5]
            r1: a ≠ c   [s6]
            r2: a = c ∨ Cons(a, Nil()) ≠ Cons(c, Nil())   [ADT: (Injectivity (constructor ((datatype ((name list))) (name Cons) (arity 2)))
     (left_args
      ((Var a)
       (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0)) ())))
     (right_args
      ((Var c)
       (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0)) ())))
     (field_index 0))]
            r3: ⊥   [RUP over [r0, r1, r2]]
    Conclusion: s7

    disjointness: check = true
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: t : list()
      a4: h : list()
      a5: Nil() = Cons(h, t)
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: t : list()   [assumption a3]
      s4: h : list()   [assumption a4]
      s5: Nil() = Cons(h, t)   [assumption a5]
      s6: false   [refutation of [s0, s1, s2, s3, s4, s5]]
        refutation:
          steps:
            r0: Nil() = Cons(h, t)   [s5]
            r1: Nil() ≠ Cons(h, t)   [ADT: (Disjointness
     (left_constructor ((datatype ((name list))) (name Nil) (arity 0)))
     (left_args ())
     (right_constructor ((datatype ((name list))) (name Cons) (arity 2)))
     (right_args ((Var h) (Var t))))]
            r2: ⊥   [RUP over [r0, r1]]
    Conclusion: s6

    selector: check = true
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: h : list()
      a4: head(Cons(h, Nil())) ≠ h
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: h : list()   [assumption a3]
      s4: head(Cons(h, Nil())) ≠ h   [assumption a4]
      s5: false   [refutation of [s0, s1, s2, s3, s4]]
        refutation:
          steps:
            r0: h ≠ head(Cons(h, Nil()))   [s4]
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
    Conclusion: s5

    tester: check = true
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: h : list()
      a4: is-Nil(Cons(h, Nil()))
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: h : list()   [assumption a3]
      s4: is-Nil(Cons(h, Nil()))   [assumption a4]
      s5: false   [refutation of [s0, s1, s2, s3, s4]]
        refutation:
          steps:
            r0: true = is-Nil(Cons(h, Nil()))   [s4]
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
    Conclusion: s5

    acyclicity: check = true
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: x : list()
      a4: x = Cons(x, Nil())
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: x : list()   [assumption a3]
      s4: x = Cons(x, Nil())   [assumption a4]
      s5: false   [refutation of [s0, s1, s2, s3, s4]]
        refutation:
          steps:
            r0: x = Cons(x, Nil())   [s4]
            r1: x ≠ Cons(x, Nil())   [ADT: (Acyclicity
     (cycle
      (((constructor_term
         (Datatype_constructor ((datatype ((name list))) (name Cons) (arity 2))
          ((Var x)
           (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0))
            ()))))
        (field (Var x))))))]
            r2: ⊥   [RUP over [r0, r1]]
    Conclusion: s5
    |}]
;;

let%expect_test "bogus ADT certificates are rejected" =
  let arg = v "arg" in
  let h = v "h" in
  let t = v "t" in
  let a = v "a" in
  let c = v "c" in
  let check ?(datatype_env = Datatype.Env.empty) clause certificate =
    Proof.check_theory_certificate ~datatype_env ~clause (Adt certificate)
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
  let tester_exclusivity_clause =
    clause_exn
      [ theory_literal (`Eq (is_nil a, Formula.True)) ~positive:false
      ; theory_literal (`Eq (is_cons a, Formula.True)) ~positive:false
      ]
  in
  let enum_completeness_clause =
    clause_exn
      [ theory_literal (`Eq (a, red)) ~positive:true
      ; theory_literal (`Eq (a, green)) ~positive:true
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
           : bool)
        ~tester_exclusivity_same_constructor:
          (Or_error.is_error
             (check
                ~datatype_env
                tester_exclusivity_clause
                (Tester_exclusivity
                   { left_constructor = nil_constructor
                   ; left_argument = a
                   ; right_constructor = nil_constructor
                   ; right_argument = a
                   }))
           : bool)
        ~tester_reconstruction_wrong_clause:
          (Or_error.is_error
             (check
                ~datatype_env
                tester_exclusivity_clause
                (Tester_reconstruction
                   { constructor = nil_constructor; argument = a }))
           : bool)
        ~completeness_missing_constructor:
          (Or_error.is_error
             (check
                ~datatype_env:color_env
                enum_completeness_clause
                (Completeness
                   { declaration = color_declaration
                   ; subject = a
                   ; guard = None
                   ; form = Enum_equalities
                   }))
           : bool)
        ~completeness_undeclared:
          (Or_error.is_error
             (check
                enum_completeness_clause
                (Completeness
                   { declaration = color_declaration
                   ; subject = a
                   ; guard = None
                   ; form = Enum_equalities
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
     (acyclicity_field_not_in_args true) (injectivity_field_out_of_bounds true)
     (tester_exclusivity_same_constructor true)
     (tester_reconstruction_wrong_clause true)
     (completeness_missing_constructor true) (completeness_undeclared true))
    (Error "ADT certificate does not match its clause")
    |}]
;;
