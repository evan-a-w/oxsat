open! Core
open! Feel
open! Ds

[@@@warning "-69"]

let%expect_test "two compatible unit clauses" =
  let solver = Solver.create () in
  (* Add clauses: x1 and x2 *)
  Helpers.add_clause_exn solver ~clause:[| 1 |];
  Helpers.add_clause_exn solver ~clause:[| 2 |];
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s
       [%message "SAT" (Helpers.assignment_literals assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Helpers.assignment_literals assignments" (1 2))) |}]
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
  let result = Helpers.solve_formula [| [||] |] in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| UNSAT |}]
;;

let%expect_test "single unit clause is satisfiable" =
  let solver = Solver.create () in
  (* Add clause: x1 (variable 1 must be true) *)
  Helpers.add_clause_exn solver ~clause:[| 1 |];
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s
       [%message "SAT" (Helpers.assignment_literals assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Helpers.assignment_literals assignments" (1))) |}]
;;

let%expect_test "contradictory unit clauses are unsatisfiable" =
  let result = Helpers.solve_formula [| [| 1 |]; [| -1 |] |] in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat { unsat_core } -> print_s [%message "UNSAT" (unsat_core : int array)]);
  [%expect {| (UNSAT (unsat_core (1))) |}]
;;

let%expect_test "simple two-literal clause" =
  let solver = Solver.create () in
  (* Add clause: x1 or x2 *)
  Helpers.add_clause_exn solver ~clause:[| 1; 2 |];
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s
       [%message "SAT" (Helpers.assignment_literals assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Helpers.assignment_literals assignments" (1 2))) |}]
;;

let%expect_test "simple two-literal clause neg" =
  let solver = Solver.create () in
  (* Add clause: x1 or x2 *)
  Helpers.add_clause_exn solver ~clause:[| -1; -2 |];
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s
       [%message "SAT" (Helpers.assignment_literals assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Helpers.assignment_literals assignments" (-1 2))) |}]
;;

let%expect_test "simple satisfiable formula" =
  let solver = Solver.create () in
  (* Add clauses: (x1 or x2) and (-x1 or x3) *)
  Helpers.add_clause_exn solver ~clause:[| 1; 2 |];
  Helpers.add_clause_exn solver ~clause:[| -1; 3 |];
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s
       [%message "SAT" (Helpers.assignment_literals assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Helpers.assignment_literals assignments" (1 2 3))) |}]
;;

let%expect_test "unit propagation leads to satisfaction" =
  let solver = Solver.create () in
  (* Add clauses that force assignments through unit propagation: x1 (-x1 or x2)
     This should force x2 to be true *)
  Helpers.add_clause_exn solver ~clause:[| 1 |];
  Helpers.add_clause_exn solver ~clause:[| -1; 2 |];
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s
       [%message "SAT" (Helpers.assignment_literals assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Helpers.assignment_literals assignments" (1 2))) |}]
;;

let%expect_test "unit propagation leads to conflict" =
  let result = Helpers.solve_formula [| [| 1 |]; [| -1; 2 |]; [| -2 |] |] in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat { unsat_core } -> print_s [%message "UNSAT" (unsat_core : int array)]);
  [%expect {| (UNSAT (unsat_core (2))) |}]
;;

let%expect_test "three-variable satisfiable formula" =
  let solver = Solver.create () in
  (* Add clauses: (x1 or x2) and (-x1 or x3) and (-x2 or -x3) *)
  Helpers.add_clause_exn solver ~clause:[| 1; 2 |];
  Helpers.add_clause_exn solver ~clause:[| -1; 3 |];
  Helpers.add_clause_exn solver ~clause:[| -2; -3 |];
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s
       [%message "SAT" (Helpers.assignment_literals assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Helpers.assignment_literals assignments" (1 -2 3))) |}]
;;

let%expect_test "pigeonhole principle - 2 pigeons, 1 hole (unsat)" =
  let result = Helpers.solve_formula [| [| 1 |]; [| 2 |]; [| -1; -2 |] |] in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat { unsat_core } -> print_s [%message "UNSAT" (unsat_core : int array)]);
  [%expect {| (UNSAT (unsat_core (1 2))) |}]
;;

let%expect_test "clause with multiple literals satisfied by one assignment" =
  let solver = Solver.create () in
  (* Add clauses: x1 and (x1 or x2 or x3) *)
  Helpers.add_clause_exn solver ~clause:[| 1 |];
  Helpers.add_clause_exn solver ~clause:[| 1; 2; 3 |];
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s
       [%message "SAT" (Helpers.assignment_literals assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Helpers.assignment_literals assignments" (1 2 3))) |}]
;;

let%expect_test "larger satisfiable formula requiring backtracking" =
  let solver = Solver.create () in
  Helpers.add_clause_exn solver ~clause:[| 1; 2 |];
  Helpers.add_clause_exn solver ~clause:[| -1; 3 |];
  Helpers.add_clause_exn solver ~clause:[| -1; -3 |];
  Helpers.add_clause_exn solver ~clause:[| -1; -2 |];
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s
       [%message "SAT" (Helpers.assignment_literals assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("Helpers.assignment_literals assignments" (-1 2 3))) |}]
;;

let%expect_test "larger unsatisfiable formula requiring backtracking" =
  let result =
    Helpers.solve_formula
      [| [| 1; 2 |]; [| 1; -2 |]; [| -1; 3 |]; [| -1; -3 |] |]
  in
  (match result with
   | Sat { assignments } ->
     print_s
       [%message "SAT" (Helpers.assignment_literals assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| UNSAT |}]
;;

let run_dimacs s =
  let result =
    Examples.Dimacs.read_string s
    |> List.map ~f:Array.of_list
    |> Array.of_list
    |> Helpers.solve_formula
  in
  match result with
  | Sat _ -> print_endline "SAT"
  | Unsat { unsat_core } ->
    print_s [%message "UNSAT" ~unsat_core:(unsat_core : int array)]
;;

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
  [%expect {| (UNSAT (unsat_core (91 90 85 -96 88 105))) |}]
;;

let%expect_test "assumptions" =
  let solver = Solver.create () in
  Helpers.add_clause_exn solver ~clause:[| 3; -5; 6 |];
  Helpers.add_clause_exn solver ~clause:[| -2; -5; -3; 6; -4 |];
  Helpers.add_clause_exn solver ~clause:[| -5; 1; 4; -6 |];
  Helpers.add_clause_exn solver ~clause:[| 3; -4; 6; 1; 2; 4 |];
  Helpers.add_clause_exn solver ~clause:[| -3; 4; -2; 6; -1; -5 |];
  Helpers.add_clause_exn solver ~clause:[| 3; -2; -6; 4 |];
  Helpers.add_clause_exn solver ~clause:[| 3; 2; -1 |];
  Helpers.add_clause_exn solver ~clause:[| -6; -4; 5; -3 |];
  Helpers.add_clause_exn solver ~clause:[| -3; 2; 5; 6; -1; -4 |];
  Helpers.add_clause_exn solver ~clause:[| 4; -2; -3; 5 |];
  Helpers.add_clause_exn solver ~clause:[| 3; -2; -1; -5; -6; -4 |];
  Helpers.add_clause_exn solver ~clause:[| -2; -6 |];
  Helpers.add_clause_exn solver ~clause:[| -1; -2; 4; 5 |];
  Helpers.add_clause_exn solver ~clause:[| 2; -4; 1; 3; -5; -6 |];
  let solve ?assumptions () =
    let result = Solver.solve ?assumptions solver in
    match result with
    | Sat { assignments } ->
      print_s
        [%message
          "SAT"
            ~assignments:(Helpers.assignment_literals assignments : int array)]
    | Unsat { unsat_core } ->
      print_s [%message "UNSAT" ~unsat_core:(unsat_core : int array)]
  in
  solve ();
  solve ~assumptions:[| 1 |] ();
  solve ~assumptions:[| 1; 2 |] ();
  solve ~assumptions:[| 1; 2; 5 |] ();
  solve ~assumptions:[| 6 |] ();
  solve ~assumptions:[| 1; 2; 6 |] ();
  solve ~assumptions:[| -1; -2; -3; -4; -5 |] ();
  solve ~assumptions:[| -1; -2; -3; -4; -5; 6 |] ();
  [%expect
    {|
    (SAT (assignments (1 -2 3 4 5 6)))
    (SAT (assignments (1 -2 3 4 5 6)))
    (SAT (assignments (1 2 -3 4 -5 -6)))
    (UNSAT (unsat_core (2 1)))
    (SAT (assignments (-1 -2 -3 4 -5 6)))
    (UNSAT (unsat_core (2)))
    (SAT (assignments (-1 -2 -3 -4 -5 -6)))
    (SAT (assignments (-1 -2 -3 -4 -5 6)))
    |}]
;;
