open! Core
open! Feel.Import
open! Theory_core
open! Theory

(* Every satisfiable instance's returned model is independently re-checked with
   [Solver.check_model], which evaluates the asserted formulas under the model's
   atom values and cross-checks the numeric/type witnesses. *)

let v name : Formula.any = Var (Tvar.of_string name)
let x : Formula.any = v "x"
let y : Formula.any = v "y"
let z : Formula.any = v "z"
let f arg : Formula.any = App (Tvar.of_string "f", [ arg ])
let eq a b : Formula.any = Eq (a, b)
let neq a b : Formula.any = Not (eq a b)
let xv = Tvar.of_string "x"
let yv = Tvar.of_string "y"
let list_datatype = Datatype.Datatype.{ name = Tvar.of_string "list" }
let list_type = Type_expr.App (list_datatype.name, [])

let nil_constructor =
  Datatype.Constructor.
    { datatype = list_datatype; name = Tvar.of_string "Nil"; arity = 0 }
;;

let cons_constructor =
  Datatype.Constructor.
    { datatype = list_datatype; name = Tvar.of_string "Cons"; arity = 2 }
;;

let head_selector =
  Datatype.Selector.
    { constructor = cons_constructor; name = Tvar.of_string "head"; index = 0 }
;;

let list_declaration =
  Datatype.Declaration.
    { datatype = list_datatype
    ; constructors =
        [ { constructor = nil_constructor; field_types = []; selectors = [] }
        ; { constructor = cons_constructor
          ; field_types = [ list_type; list_type ]
          ; selectors = [ head_selector ]
          }
        ]
    }
;;

let list_datatype_env =
  Or_error.ok_exn (Datatype.Env.of_declarations [ list_declaration ])
;;

let adt_solver () =
  Solver.create
    ~config:{ Solver.Config.default with datatype_env = list_datatype_env }
    ()
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

let color_declaration =
  Datatype.Declaration.
    { datatype = color_datatype
    ; constructors =
        [ { constructor = red_constructor; field_types = []; selectors = [] }
        ; { constructor = green_constructor; field_types = []; selectors = [] }
        ]
    }
;;

let color_datatype_env =
  Or_error.ok_exn (Datatype.Env.of_declarations [ color_declaration ])
;;

let color_solver () =
  Solver.create
    ~config:{ Solver.Config.default with datatype_env = color_datatype_env }
    ()
;;

let red : Formula.any = Datatype_constructor (red_constructor, [])
let green : Formula.any = Datatype_constructor (green_constructor, [])
let nil : Formula.any = Datatype_constructor (nil_constructor, [])

let cons head tail : Formula.any =
  Datatype_constructor (cons_constructor, [ head; tail ])
;;

let head value : Formula.any = Datatype_selector (head_selector, value)
let is_nil value : Formula.any = Datatype_tester (nil_constructor, value)

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

let%expect_test "array row axiom violation in a corrupted model is rejected" =
  let solver = Solver.create () in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let i : Formula.any = Var (Tvar.of_string "i") in
  let value : Formula.any = Var (Tvar.of_string "value") in
  let row = Formula.Select (Store (a, i, value), i) in
  assert_ok solver (eq row row);
  (match Solver.solve solver with
   | Unsat _ -> print_endline "unexpectedly unsat"
   | Sat { model } ->
     let row_value_atom = Atom.normalize (`Eq (row, value)) in
     let corrupted =
       { model with
         Model.atom_values =
           Map.mapi model.atom_values ~f:(fun ~key ~data ->
             if [%compare.equal: Atom.t] (Atom.normalize key) row_value_atom
             then false
             else data)
       ; euf_classes = Map.mapi model.euf_classes ~f:(fun ~key ~data:_ -> key)
       }
     in
     print_s [%sexp (Solver.check_model solver corrupted : unit Or_error.t)]);
  [%expect
    {|
    (Error
     ("array read-over-write/same-index axiom is violated" (array (Var a))
      (index (Var i)) (value (Var value))))
    |}]
;;

let set_euf_class model term representative =
  { model with
    Model.euf_classes =
      Map.set model.Model.euf_classes ~key:term ~data:representative
  }
;;

let check_corrupted_model solver ~f =
  match Solver.solve solver with
  | Unsat _ -> print_endline "unexpectedly unsat"
  | Sat { model } ->
    print_s [%sexp (Solver.check_model solver (f model) : unit Or_error.t)]
;;

let%expect_test "ADT constructor disjointness violation in a corrupted model \
                 is rejected"
  =
  let solver = adt_solver () in
  let h = v "h" in
  let t = v "t" in
  let cons_ht = cons h t in
  assert_ok solver (eq nil nil);
  assert_ok solver (eq cons_ht cons_ht);
  check_corrupted_model solver ~f:(fun model -> set_euf_class model cons_ht nil);
  [%expect
    {|
    (Error
     ("ADT constructor disjointness is violated"
      (left
       (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0)) ()))
      (right
       (Datatype_constructor ((datatype ((name list))) (name Cons) (arity 2))
        ((Var h) (Var t))))))
    |}]
;;

let%expect_test "ADT constructor injectivity violation in a corrupted model is \
                 rejected"
  =
  let solver = adt_solver () in
  let a = v "a" in
  let c = v "c" in
  let left = cons a nil in
  let right = cons c nil in
  assert_ok solver (eq left left);
  assert_ok solver (eq right right);
  assert_ok solver (neq a c);
  check_corrupted_model solver ~f:(fun model -> set_euf_class model right left);
  [%expect
    {|
    (Error
     ("ADT constructor injectivity is violated"
      (left
       (Datatype_constructor ((datatype ((name list))) (name Cons) (arity 2))
        ((Var a)
         (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0))
          ()))))
      (right
       (Datatype_constructor ((datatype ((name list))) (name Cons) (arity 2))
        ((Var c)
         (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0))
          ()))))))
    |}]
;;

let%expect_test "ADT selector projection violation in a corrupted model is \
                 rejected"
  =
  let solver = adt_solver () in
  let h = v "h" in
  let other = v "other" in
  let cons_h = cons h nil in
  let selector = head cons_h in
  assert_ok solver (eq selector selector);
  assert_ok solver (neq h other);
  check_corrupted_model solver ~f:(fun model ->
    { (set_euf_class model selector other) with
      Model.atom_values =
        Map.set
          model.Model.atom_values
          ~key:(Atom.normalize (`Eq (h, selector)))
          ~data:false
    });
  [%expect
    {|
    (Error
     ("ADT selector projection is violated"
      (selector_term
       (Datatype_selector
        ((constructor ((datatype ((name list))) (name Cons) (arity 2)))
         (name head) (index 0))
        (Datatype_constructor ((datatype ((name list))) (name Cons) (arity 2))
         ((Var h)
          (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0))
           ())))))
      (projected (Var h))))
    |}]
;;

let%expect_test "ADT tester value violation in a corrupted model is rejected" =
  let solver = adt_solver () in
  let h = v "h" in
  let cons_h = cons h nil in
  let tester = is_nil cons_h in
  assert_ok solver (eq tester tester);
  assert_ok solver (eq True True);
  check_corrupted_model solver ~f:(fun model ->
    { (set_euf_class model tester True) with
      Model.atom_values =
        Map.set
          model.Model.atom_values
          ~key:(Atom.normalize (`Eq (tester, True)))
          ~data:true
    });
  [%expect
    {|
    (Error
     ("ADT tester value is violated"
      (tester
       (Datatype_tester ((datatype ((name list))) (name Nil) (arity 0))
        (Datatype_constructor ((datatype ((name list))) (name Cons) (arity 2))
         ((Var h)
          (Datatype_constructor ((datatype ((name list))) (name Nil) (arity 0))
           ())))))
      (expected false)))
    |}]
;;

let%expect_test "ADT acyclicity violation in a corrupted model is rejected" =
  let solver = adt_solver () in
  let x = v "x" in
  let cons_x = cons x nil in
  assert_ok solver (eq cons_x cons_x);
  check_corrupted_model solver ~f:(fun model -> set_euf_class model x cons_x);
  [%expect {| (Error "ADT acyclicity is violated") |}]
;;

let%expect_test "ADT enum exhaustiveness violation in a corrupted model is \
                 rejected"
  =
  let solver = color_solver () in
  assert_ok solver (neq x red);
  check_corrupted_model solver ~f:(fun model ->
    { (set_euf_class
         (set_euf_class (set_euf_class model x x) red red)
         green
         green)
      with
      Model.atom_values =
        model.atom_values
        |> Map.set ~key:(Atom.normalize (`Eq (x, red))) ~data:false
        |> Map.set ~key:(Atom.normalize (`Eq (x, green))) ~data:false
    });
  [%expect
    {|
    (Error
     ("ADT enum exhaustiveness is violated" (subject (Var x))
      (datatype ((name color)))))
    |}]
;;

let%expect_test "ADT constructor completeness violation in a corrupted model \
                 is rejected"
  =
  let solver = adt_solver () in
  assert_ok solver (Not (is_nil x));
  check_corrupted_model solver ~f:(fun model ->
    let is_nil_x = is_nil x in
    let is_cons_x = Formula.Datatype_tester (cons_constructor, x) in
    { (set_euf_class
         (set_euf_class model is_nil_x is_nil_x)
         is_cons_x
         is_cons_x)
      with
      Model.atom_values =
        model.atom_values
        |> Map.set
             ~key:(Atom.normalize (`Eq (is_nil_x, Formula.True)))
             ~data:false
        |> Map.set
             ~key:(Atom.normalize (`Eq (is_cons_x, Formula.True)))
             ~data:false
    });
  [%expect
    {|
    (Error
     ("ADT constructor completeness is violated" (subject (Var x))
      (datatype ((name list)))))
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
