open! Core
open! Feel

let%expect_test "simple_sat finds satisfiable formula" =
  let result = Simple_sat.solve ~size:3 [| [| 1; 2 |]; [| -1; 3 |] |] in
  print_s [%sexp (result : bool)];
  [%expect {| true |}]
;;

let%expect_test "simple_sat finds unsatisfiable formula" =
  let result = Simple_sat.solve ~size:1 [| [| 1 |]; [| -1 |] |] in
  print_s [%sexp (result : bool)];
  [%expect {| false |}]
;;

let%expect_test "simple_sat solves dimacs examples" =
  print_s
    [%sexp
      { succ_eg =
          (Simple_sat.solve_dimacs_string Examples.Dimacs.succ_eg : bool)
      ; fail_eg =
          (Simple_sat.solve_dimacs_string Examples.Dimacs.fail_eg : bool)
      }];
  [%expect {| ((succ_eg true) (fail_eg false)) |}]
;;
