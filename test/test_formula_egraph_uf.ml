open! Core
open! Feel.Import
open! Theory_core
open! Theory

let x : Formula.any = Var (Tvar.of_string "x")
let y : Formula.any = Var (Tvar.of_string "y")
let f arg : Formula.any = App (Tvar.of_string "f", [ arg ])
let eq a b : Atom.Equality.t = Atom.Equality.normalize (`Eq (a, b))

let print_lemma
  (lemma : [ `Consistent | `Lemma of (Atom.Equality.t * bool) list ])
  =
  print_s
    ([%sexp_of: [ `Consistent | `Lemma of (Atom.Equality.t * bool) list ]]
       lemma)
;;

let%expect_test "congruence conflict: f(x) <> f(y) with x = y" =
  let t = Formula_egraph_uf.create ~atoms:[] in
  let fx = f x in
  let fy = f y in
  Formula_egraph_uf.add_atom t ~atom:(eq x y);
  Formula_egraph_uf.add_atom t ~atom:(eq fx fy);
  Formula_egraph_uf.assert_atom t ~decision_level:0 ~atom:(eq x y) ~value:true;
  Formula_egraph_uf.assert_atom
    t
    ~decision_level:0
    ~atom:(eq fx fy)
    ~value:false;
  print_lemma (Formula_egraph_uf.maybe_get_lemma t);
  [%expect
    {|
    (Lemma
     (((Eq ((App f ((Var x))) (App f ((Var y))))) true)
      ((Eq ((Var x) (Var y))) false)))
    |}]
;;

let%expect_test "undo: merge is rolled back to an earlier decision level" =
  let t = Formula_egraph_uf.create ~atoms:[] in
  Formula_egraph_uf.add_atom t ~atom:(eq x y);
  print_s ([%sexp_of: Formula.any] (Formula_egraph_uf.canonical_term t ~term:x));
  [%expect {| (Var x) |}];
  Formula_egraph_uf.assert_atom t ~decision_level:1 ~atom:(eq x y) ~value:true;
  print_s ([%sexp_of: Formula.any] (Formula_egraph_uf.canonical_term t ~term:y));
  [%expect {| (Var x) |}];
  Formula_egraph_uf.undo t ~to_decision_level_excl:0;
  print_s ([%sexp_of: Formula.any] (Formula_egraph_uf.canonical_term t ~term:y));
  [%expect {| (Var y) |}]
;;

let%expect_test "Type_eq atoms are handled natively" =
  let t = Formula_egraph_uf.create ~atoms:[] in
  let a = Type_expr.Var (Tvar.of_string "a") in
  let int_ : Type_expr.t = Base Int in
  let float_ : Type_expr.t = Base Float in
  Formula_egraph_uf.add_atom t ~atom:(`Type_eq (a, int_));
  Formula_egraph_uf.add_atom t ~atom:(`Type_eq (a, float_));
  Formula_egraph_uf.add_atom t ~atom:(`Type_eq (int_, float_));
  Formula_egraph_uf.assert_atom
    t
    ~decision_level:0
    ~atom:(`Type_eq (int_, float_))
    ~value:false;
  Formula_egraph_uf.assert_atom
    t
    ~decision_level:0
    ~atom:(`Type_eq (a, int_))
    ~value:true;
  Formula_egraph_uf.assert_atom
    t
    ~decision_level:0
    ~atom:(`Type_eq (a, float_))
    ~value:true;
  print_lemma (Formula_egraph_uf.maybe_get_lemma t);
  [%expect
    {|
    (Lemma
     (((Type_eq ((Base Int) (Base Float))) true)
      ((Type_eq ((Var a) (Base Int))) false)
      ((Type_eq ((Var a) (Base Float))) false)))
    |}]
;;

let%expect_test "add_term registers raw formula shapes for e-matching" =
  let t = Formula_egraph_uf.create ~atoms:[] in
  Formula_egraph_uf.add_term t ~term:(Not (Eq (f x, y)));
  let graph = Formula_egraph_uf.egraph t in
  let pattern : Formula_egraph.Pattern.Query.t =
    App (Formula.Op.App (Tvar.of_string "f"), [ Var "arg" ])
  in
  let matches = Formula_egraph.Pattern.Query.search pattern ~graph in
  List.iter
    matches
    ~f:(fun ({ eclass; subst } : Formula_egraph.Pattern.Match.t) ->
      let arg =
        Option.bind
          (Formula_egraph.Pattern.Subst.find subst "arg")
          ~f:(Formula_egraph_uf.term_of_id t)
      in
      print_s
        [%message
          ""
            ~matched:
              (Formula_egraph_uf.term_of_id t eclass : Formula.any option)
            (arg : Formula.any option)]);
  [%expect {| ((matched ((App f ((Var x))))) (arg ((Var x)))) |}]
;;

let%expect_test "Formula_egraph.Pattern.Query.search finds registered shapes" =
  let t = Formula_egraph_uf.create ~atoms:[] in
  let fx = f x in
  Formula_egraph_uf.add_atom t ~atom:(eq fx fx);
  let graph = Formula_egraph_uf.egraph t in
  let pattern : Formula_egraph.Pattern.Query.t =
    App (Formula.Op.App (Tvar.of_string "f"), [ Var "arg" ])
  in
  let matches = Formula_egraph.Pattern.Query.search pattern ~graph in
  print_s [%sexp (List.length matches : int)];
  [%expect {| 1 |}]
;;
