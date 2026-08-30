open! Core
open! Feel.Import
open! Theory_core

let x = Tvar.of_string "x"
let y = Tvar.of_string "y"
let z = Tvar.of_string "z"
let f = Tvar.of_string "f"
let vx : Formula.any = Var x
let vy : Formula.any = Var y
let vz : Formula.any = Var z
let datatype = Datatype.Datatype.{ name = Tvar.of_string "list" }

let nil =
  Datatype.Constructor.{ datatype; name = Tvar.of_string "Nil"; arity = 0 }
;;

let cons =
  Datatype.Constructor.{ datatype; name = Tvar.of_string "Cons"; arity = 2 }
;;

let head =
  Datatype.Selector.
    { constructor = cons; name = Tvar.of_string "head"; index = 0 }
;;

let q q = Formula.widen_quantified q
let qvx = q vx
let qvy = q vy
let qvz = q vz

let nested_quantified () : Formula.quantified =
  Forall
    ( [ x ]
    , [ [ App (f, [ qvx ]) ] ]
    , Exists ([ y ], Eq (App (f, [ qvx; qvy ]), qvz)) )
;;

let representatives : Formula.quantified list =
  [ q vx
  ; q (Eq (vx, vy))
  ; q (Ite (True, vx, vy))
  ; q True
  ; q False
  ; q (Not True)
  ; q (And [ True ])
  ; q (Or [ False ])
  ; Formula.widen_quantified (Forall ([ x ], [ [ qvx ] ], Eq (qvx, qvx)))
  ; Formula.widen_quantified (Exists ([ x ], Eq (qvx, qvx)))
  ; q (App (Tvar.of_string "f", [ vx ]))
  ; q (Select (vx, vy))
  ; q (Store (vx, vy, vx))
  ; q (Datatype_constructor (nil, []))
  ; q (Datatype_selector (head, vx))
  ; q (Datatype_tester (nil, vx))
  ; q Bool
  ; q Int
  ; q Real
  ; q Int64
  ; q Type
  ; q (Function_type (Int, Int))
  ; q (Array_type (Int, Int))
  ; q (Type_of vx)
  ; q (Type_var x)
  ; q (Type_app (Tvar.of_string "ty", [ Int ]))
  ; q (La_const Q.zero)
  ; q (La_scale_const (Q.one, vx))
  ; q (La_add (vx, vy))
  ; q (La_compare (vx, `Le, vy))
  ]
;;

let%expect_test "formula compare/rank distinguishes constructor representatives"
  =
  let set = Formula.Quantified.Set.of_list representatives in
  print_s
    [%message
      (List.length representatives : int)
        ~unique:(Set.length set : int)
        ~duplicates:(List.length representatives - Set.length set : int)];
  [%expect
    {| (("List.length representatives" 30) (unique 30) (duplicates 0)) |}]
;;

let%expect_test "type lattice numeric bases" =
  let show type_expr = [%sexp (type_expr : Type_expr.t)] in
  let meet a b = Option.map (Type_lattice.meet a b) ~f:show in
  print_s
    [%message
      "lattice"
        ~int64_int:(Type_lattice.is_subtype (Base Int64) ~of_:(Base Int) : bool)
        ~int_real:(Type_lattice.is_subtype (Base Int) ~of_:(Base Real) : bool)
        ~int64_real:
          (Type_lattice.is_subtype (Base Int64) ~of_:(Base Real) : bool)
        ~int_real_exact:
          (Type_lattice.is_subtype (Base Real) ~of_:(Base Int) : bool)
        ~meet_int64_int:(meet (Base Int64) (Base Int) : Sexp.t option)
        ~meet_int_real:(meet (Base Int) (Base Real) : Sexp.t option)
        ~meet_int64_real:(meet (Base Int64) (Base Real) : Sexp.t option)
        ~bool_int_disjoint:(Type_lattice.disjoint (Base Bool) (Base Int) : bool)];
  [%expect
    {|
    (lattice (int64_int true) (int_real true) (int64_real true)
     (int_real_exact false) (meet_int64_int ((Base Int64)))
     (meet_int_real ((Base Int))) (meet_int64_real ((Base Int64)))
     (bool_int_disjoint true))
    |}]
;;

let%expect_test "nested quantifier sexp" =
  print_s (Formula.sexp_of_quantified (nested_quantified ()));
  [%expect
    {|
    (Forall (x) (((App f ((Var x)))))
     (Exists (y) (Eq (App f ((Var x) (Var y))) (Var z))))
    |}]
;;

let%expect_test "nested quantifier tvars" =
  print_s [%sexp (Formula.tvars (nested_quantified ()) : Tvar.Set.t)];
  [%expect {| (x y z f) |}]
;;

let%expect_test "to_any distinguishes nested quantified and ground formulas" =
  print_s
    [%message
      "to_any"
        ~nested:(Formula.to_any (nested_quantified ()) : Formula.any option)
        ~ground:(Formula.to_any (q (Eq (vx, vy))) : Formula.any option)];
  [%expect {| (to_any (nested ()) (ground ((Eq (Var x) (Var y))))) |}]
;;

let%expect_test "nested quantified sexp round trip" =
  let formula = nested_quantified () in
  let round_trip =
    Formula.quantified_of_sexp (Formula.sexp_of_quantified formula)
  in
  print_s
    [%message
      (Formula.equal_quantified formula round_trip : bool)
        ~round_trip:(round_trip : Formula.quantified)];
  [%expect
    {|
    (("Formula.equal_quantified formula round_trip" true)
     (round_trip
      (Forall (x) (((App f ((Var x)))))
       (Exists (y) (Eq (App f ((Var x) (Var y))) (Var z))))))
    |}]
;;
