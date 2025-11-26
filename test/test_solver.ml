open! Core
open! Feel
open! Ds

[@@@warning "-69"]

let%expect_test "two compatible unit clauses" =
  let solver = Solver.create () in
  (* Add clauses: x1 and x2 *)
  let _ = Solver.add_clause' solver ~clause:[| 1 |] in
  let _ = Solver.add_clause' solver ~clause:[| 2 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (Clause.to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Clause.to_int_array assignments" (1 2))) |}]
;;

let%expect_test "empty solver is satisfiable" =
  let solver = Solver.create () in
  let result = Solver.solve solver in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| SAT |}]
;;

let%expect_test "empty clause is unsatisfiable" =
  let solver = Solver.create () in
  let _ = Solver.add_clause' solver ~clause:[||] in
  let result = Solver.solve solver in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| UNSAT |}]
;;

let%expect_test "single unit clause is satisfiable" =
  let solver = Solver.create () in
  (* Add clause: x1 (variable 1 must be true) *)
  let _ = Solver.add_clause' solver ~clause:[| 1 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (Clause.to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Clause.to_int_array assignments" (1))) |}]
;;

let%expect_test "contradictory unit clauses are unsatisfiable" =
  let solver = Solver.create () in
  (* Add clauses: x1 and -x1 (contradiction) *)
  let _ = Solver.add_clause' solver ~clause:[| 1 |] in
  let _ = Solver.add_clause' solver ~clause:[| -1 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat { unsat_core } ->
     print_s [%message "UNSAT" (Clause.to_int_array unsat_core : int array)]);
  [%expect {| (UNSAT ("Clause.to_int_array unsat_core" (-1))) |}]
;;

let%expect_test "simple two-literal clause" =
  let solver = Solver.create () in
  (* Add clause: x1 or x2 *)
  let _ = Solver.add_clause' solver ~clause:[| 1; 2 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (Clause.to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Clause.to_int_array assignments" (1 2))) |}]
;;

let%expect_test "simple two-literal clause neg" =
  let solver = Solver.create () in
  (* Add clause: x1 or x2 *)
  let _ = Solver.add_clause' solver ~clause:[| -1; -2 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (Clause.to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Clause.to_int_array assignments" (-1 2))) |}]
;;

let%expect_test "simple satisfiable formula" =
  let solver = Solver.create () in
  (* Add clauses: (x1 or x2) and (-x1 or x3) *)
  let _ = Solver.add_clause' solver ~clause:[| 1; 2 |] in
  let _ = Solver.add_clause' solver ~clause:[| -1; 3 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (Clause.to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Clause.to_int_array assignments" (1 2 3))) |}]
;;

let%expect_test "unit propagation leads to satisfaction" =
  let solver = Solver.create () in
  (* Add clauses that force assignments through unit propagation:
     x1
     (-x1 or x2)
     This should force x2 to be true *)
  let _ = Solver.add_clause' solver ~clause:[| 1 |] in
  let _ = Solver.add_clause' solver ~clause:[| -1; 2 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (Clause.to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Clause.to_int_array assignments" (1 2))) |}]
;;

let%expect_test "unit propagation leads to conflict" =
  let solver = Solver.create () in
  (* Add clauses that lead to contradiction:
     x1
     (-x1 or x2)
     -x2 *)
  let _ = Solver.add_clause' solver ~clause:[| 1 |] in
  let _ = Solver.add_clause' solver ~clause:[| -1; 2 |] in
  let _ = Solver.add_clause' solver ~clause:[| -2 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat { unsat_core } ->
     print_s [%message "UNSAT" (Clause.to_int_array unsat_core : int array)]);
  [%expect {| (UNSAT ("Clause.to_int_array unsat_core" (-2))) |}]
;;

let%expect_test "three-variable satisfiable formula" =
  let solver = Solver.create () in
  (* Add clauses: (x1 or x2) and (-x1 or x3) and (-x2 or -x3) *)
  let _ = Solver.add_clause' solver ~clause:[| 1; 2 |] in
  let _ = Solver.add_clause' solver ~clause:[| -1; 3 |] in
  let _ = Solver.add_clause' solver ~clause:[| -2; -3 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (Clause.to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Clause.to_int_array assignments" (1 -2 3))) |}]
;;

let%expect_test "pigeonhole principle - 2 pigeons, 1 hole (unsat)" =
  let solver = Solver.create () in
  (* Variables: x_ij means pigeon i is in hole j
     x_11 (var 1): pigeon 1 in hole 1
     x_21 (var 2): pigeon 2 in hole 1

     Clauses:
     - Each pigeon must be in some hole: x_11, x_21
     - No two pigeons in same hole: -x_11 or -x_21 *)
  let _ = Solver.add_clause' solver ~clause:[| 1 |] in
  let _ = Solver.add_clause' solver ~clause:[| 2 |] in
  let _ = Solver.add_clause' solver ~clause:[| -1; -2 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat { unsat_core } ->
     print_s [%message "UNSAT" (Clause.to_int_array unsat_core : int array)]);
  [%expect {| (UNSAT ("Clause.to_int_array unsat_core" (-1))) |}]
;;

let%expect_test "clause with multiple literals satisfied by one assignment" =
  let solver = Solver.create () in
  (* Add clauses: x1 and (x1 or x2 or x3) *)
  let _ = Solver.add_clause' solver ~clause:[| 1 |] in
  let _ = Solver.add_clause' solver ~clause:[| 1; 2; 3 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (Clause.to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Clause.to_int_array assignments" (1 2 3))) |}]
;;

let%expect_test "larger satisfiable formula requiring backtracking" =
  let solver = Solver.create () in
  let _ = Solver.add_clause' solver ~clause:[| 1; 2 |] in
  let _ = Solver.add_clause' solver ~clause:[| -1; 3 |] in
  let _ = Solver.add_clause' solver ~clause:[| -1; -3 |] in
  let _ = Solver.add_clause' solver ~clause:[| -1; -2 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (Clause.to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Clause.to_int_array assignments" (-1 2 3))) |}]
;;

let%expect_test "larger unsatisfiable formula requiring backtracking" =
  let solver = Solver.create () in
  let _ = Solver.add_clause' solver ~clause:[| 1; 2 |] in
  let _ = Solver.add_clause' solver ~clause:[| 1; -2 |] in
  let _ = Solver.add_clause' solver ~clause:[| -1; 3 |] in
  let _ = Solver.add_clause' solver ~clause:[| -1; -3 |] in
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (Clause.to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| UNSAT |}]
;;


let run_dimacs s =
  let solver = Solver.create () in
  List.iter (Examples.Dimacs.read_string s) ~f:(fun clause ->
      let clause = Array.of_list clause in
      ignore (Solver.add_clause' solver ~clause : Solver.t));
  let result = Solver.solve solver in
  (match result with
   | Sat _ ->
     print_endline "SAT"
   | Unsat { unsat_core } -> print_s [%message "UNSAT" ~unsat_core:(Clause.to_int_array unsat_core : int array)])

let%expect_test "sudoku" =
  run_dimacs Examples.Dimacs.sudoku;
  (* should be SAT *)
  [%expect {| SAT |}]
;;

let%expect_test "succ dimacs" =
  run_dimacs Examples.Dimacs.succ_eg;
  [%expect {| SAT |}]
;;

let%expect_test "fail dimacs" =
  run_dimacs Examples.Dimacs.fail_eg;
  [%expect {| (UNSAT (unsat_core (-91))) |}]
;;
