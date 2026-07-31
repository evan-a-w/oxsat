open! Core
open! Feel.Import
open! Theory_core

let x = Tvar.of_string "x"
let y = Tvar.of_string "y"
let vx : Formula.any = Var x
let vy : Formula.any = Var y
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

let representatives : Formula.quantified list =
  [ q vx
  ; q (Eq (vx, vy))
  ; q (Ite (True, vx, vy))
  ; q True
  ; q False
  ; q (Not True)
  ; q (And [ True ])
  ; q (Or [ False ])
  ; Formula.widen_quantified (Forall ([ x ], [ [ vx ] ], Eq (vx, vx)))
  ; Formula.widen_quantified (Exists ([ x ], Eq (vx, vx)))
  ; q (App (Tvar.of_string "f", [ vx ]))
  ; q (Select (vx, vy))
  ; q (Store (vx, vy, vx))
  ; q (Datatype_constructor (nil, []))
  ; q (Datatype_selector (head, vx))
  ; q (Datatype_tester (nil, vx))
  ; q Bool
  ; q Int
  ; q Float
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

let%expect_test "formula compare distinguishes constructor representatives" =
  let set = Formula.Quantified.Set.of_list representatives in
  print_s
    [%message
      (List.length representatives : int)
        ~unique:(Set.length set : int)
        ~duplicates:(List.length representatives - Set.length set : int)];
  [%expect
    {| (("List.length representatives" 29) (unique 29) (duplicates 0)) |}]
;;
