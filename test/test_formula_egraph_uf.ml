open! Core
open! Feel.Import
open! Theory_core
open! Theory

let x : Formula.any = Var (Tvar.of_string "x")
let y : Formula.any = Var (Tvar.of_string "y")
let f arg : Formula.any = App (Tvar.of_string "f", [ arg ])

let eq a b : Formula_egraph_uf.Atom.t =
  Formula_egraph_uf.Atom.normalize (`Eq (a, b))
;;

let print_lemma
  (lemma : [ `Consistent | `Lemma of (Formula_egraph_uf.Atom.t * bool) list ])
  =
  print_s
    ([%sexp_of:
       [ `Consistent | `Lemma of (Formula_egraph_uf.Atom.t * bool) list ]]
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
