open! Core
open! Feel
open! Ds

[@@@warning "-69"]

let assignments_to_int_array assignments =
  let literals = ref [] in
  for var = 1 to Array.length assignments - 1 do
    match assignments.(var) with
    | None -> ()
    | Some true -> literals := var :: !literals
    | Some false -> literals := -var :: !literals
  done;
  Array.of_list_rev !literals
;;

let%expect_test "solve times out when bounded timer is exhausted" =
  let solver = Solver.create () in
  (try
     ignore (Solver.solve ~time_bound:(`Bounded 0) solver : Sat_result.t);
     print_endline "finished"
   with
   | Solver.Timeout ->
     print_s [%message "timeout" ~stats:(Solver.stats solver : Stats.t)]);
  [%expect
    {|
    (timeout
     (stats
      ((iterations 1) (decisions 0) (propagations 0) (conflicts 0)
       (learned_clauses 0) (learned_clause_literals 0) (max_decision_level 0)
       (deleted_clauses 0) (restarts 0))))
    |}]
;;

let%expect_test "two compatible unit clauses" =
  let solver = Solver.create () in
  (* Add clauses: x1 and x2 *)
  ignore (Solver.add_clause solver ~clause:[| 1 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| 2 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (assignments_to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("assignments_to_int_array assignments" (1 2))) |}]
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
  ignore (Solver.add_clause solver ~clause:[||] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| UNSAT |}]
;;

let%expect_test "single unit clause is satisfiable" =
  let solver = Solver.create () in
  (* Add clause: x1 (variable 1 must be true) *)
  ignore (Solver.add_clause solver ~clause:[| 1 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (assignments_to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("assignments_to_int_array assignments" (1))) |}]
;;

let%expect_test "contradictory unit clauses are unsatisfiable" =
  let solver = Solver.create () in
  (* Add clauses: x1 and -x1 (contradiction) *)
  ignore (Solver.add_clause solver ~clause:[| 1 |] : [ `Ok | `Unsat of _ ]);
  let result =
    match Solver.add_clause solver ~clause:[| -1 |] with
    | `Unsat core -> Sat_result.Unsat { proof = core }
    | `Ok -> Solver.solve solver
  in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat { proof } ->
     print_s [%message "UNSAT" (proof : Sat_result.Proof_clause.t list)]);
  [%expect
    {|
    (UNSAT
     (proof
      (((literals (-1)) (is_theory false)) ((literals (1)) (is_theory false)))))
    |}]
;;

let%expect_test "simple two-literal clause" =
  let solver = Solver.create () in
  (* Add clause: x1 or x2 *)
  ignore (Solver.add_clause solver ~clause:[| 1; 2 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (assignments_to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("assignments_to_int_array assignments" (1 2))) |}]
;;

let%expect_test "contradictory assumptions are unsat" =
  let solver = Solver.create () in
  let result = Solver.solve solver ~assumptions:[| 1; -1 |] in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (assignments_to_int_array assignments : int array)]
   | Unsat { proof } ->
     print_s [%message "UNSAT" (proof : Sat_result.Proof_clause.t list)]);
  [%expect {| (UNSAT (proof (((literals (1 -1)) (is_theory false))))) |}]
;;

let%expect_test "adding clause after solve can change model" =
  let solver = Solver.create () in
  ignore (Solver.add_clause solver ~clause:[| 1; 2 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s
       [%message "before" (assignments_to_int_array assignments : int array)]
   | Unsat _ -> print_endline "before UNSAT");
  ignore (Solver.add_clause solver ~clause:[| -1 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s
       [%message "after" (assignments_to_int_array assignments : int array)]
   | Unsat { proof } ->
     print_s [%message "after UNSAT" (proof : Sat_result.Proof_clause.t list)]);
  [%expect
    {|
    (before ("assignments_to_int_array assignments" (1 2)))
    (after ("assignments_to_int_array assignments" (-1 2)))
    |}]
;;

let%expect_test "simple two-literal clause neg" =
  let solver = Solver.create () in
  (* Add clause: x1 or x2 *)
  ignore (Solver.add_clause solver ~clause:[| -1; -2 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (assignments_to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("assignments_to_int_array assignments" (-1 2))) |}]
;;

let%expect_test "simple satisfiable formula" =
  let solver = Solver.create () in
  (* Add clauses: (x1 or x2) and (-x1 or x3) *)
  ignore (Solver.add_clause solver ~clause:[| 1; 2 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| -1; 3 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (assignments_to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("assignments_to_int_array assignments" (1 2 3))) |}]
;;

let%expect_test "unit propagation leads to satisfaction" =
  let solver = Solver.create () in
  (* Add clauses that force assignments through unit propagation: x1 (-x1 or x2)
     This should force x2 to be true *)
  ignore (Solver.add_clause solver ~clause:[| 1 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| -1; 2 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (assignments_to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("assignments_to_int_array assignments" (1 2))) |}]
;;

let%expect_test "unit propagation leads to conflict" =
  let solver = Solver.create () in
  (* Add clauses that lead to contradiction: x1 (-x1 or x2) -x2 *)
  let result =
    let add clause =
      match Solver.add_clause solver ~clause with
      | `Ok -> None
      | `Unsat core -> Some (Sat_result.Unsat { proof = core })
    in
    match add [| 1 |] with
    | Some r -> r
    | None ->
      (match add [| -1; 2 |] with
       | Some r -> r
       | None ->
         (match add [| -2 |] with
          | Some r -> r
          | None -> Solver.solve solver))
  in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat { proof } ->
     print_s [%message "UNSAT" (proof : Sat_result.Proof_clause.t list)]);
  [%expect
    {|
    (UNSAT
     (proof
      (((literals (-2)) (is_theory false)) ((literals (2 -1)) (is_theory false))
       ((literals (1)) (is_theory false)))))
    |}]
;;

let%expect_test "three-variable satisfiable formula" =
  let solver = Solver.create () in
  (* Add clauses: (x1 or x2) and (-x1 or x3) and (-x2 or -x3) *)
  ignore (Solver.add_clause solver ~clause:[| 1; 2 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| -1; 3 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| -2; -3 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (assignments_to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("assignments_to_int_array assignments" (1 -2 3))) |}]
;;

let%expect_test "pigeonhole principle - 2 pigeons, 1 hole (unsat)" =
  let solver = Solver.create () in
  (* Variables: x_ij means pigeon i is in hole j x_11 (var 1): pigeon 1 in hole
     1 x_21 (var 2): pigeon 2 in hole 1

     Clauses:
     - Each pigeon must be in some hole: x_11, x_21
     - No two pigeons in same hole: -x_11 or -x_21 *)
  ignore (Solver.add_clause solver ~clause:[| 1 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| 2 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| -1; -2 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat _ -> print_endline "SAT"
   | Unsat { proof } ->
     print_s [%message "UNSAT" (proof : Sat_result.Proof_clause.t list)]);
  [%expect
    {|
    (UNSAT
     (proof
      (((literals (-2 -1)) (is_theory false)) ((literals (2)) (is_theory false))
       ((literals (1)) (is_theory false)))))
    |}]
;;

let%expect_test "clause with multiple literals satisfied by one assignment" =
  let solver = Solver.create () in
  (* Add clauses: x1 and (x1 or x2 or x3) *)
  ignore (Solver.add_clause solver ~clause:[| 1 |] : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| 1; 2; 3 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (assignments_to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("assignments_to_int_array assignments" (1 2 3))) |}]
;;

let%expect_test "larger satisfiable formula requiring backtracking" =
  let solver = Solver.create () in
  ignore (Solver.add_clause solver ~clause:[| 1; 2 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| -1; 3 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| -1; -3 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| -1; -2 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (assignments_to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| (SAT ("assignments_to_int_array assignments" (-1 2 3))) |}]
;;

let%expect_test "larger unsatisfiable formula requiring backtracking" =
  let solver = Solver.create () in
  ignore (Solver.add_clause solver ~clause:[| 1; 2 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| 1; -2 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| -1; 3 |] : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| -1; -3 |] : [ `Ok | `Unsat of _ ]);
  let result = Solver.solve solver in
  (match result with
   | Sat { assignments } ->
     print_s [%message "SAT" (assignments_to_int_array assignments : int array)]
   | Unsat _ -> print_endline "UNSAT");
  [%expect {| UNSAT |}]
;;

let run_dimacs s =
  let solver = Solver.create () in
  List.iter (Examples.Dimacs.read_string s) ~f:(fun clause ->
    let clause = Array.of_list clause in
    ignore (Solver.add_clause solver ~clause : [ `Ok | `Unsat of _ ]));
  let result = Solver.solve solver in
  match result with
  | Sat _ -> print_endline "SAT"
  | Unsat { proof } ->
    print_s
      [%message "UNSAT" ~unsat_core:(proof : Sat_result.Proof_clause.t list)]
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
  [%expect
    {|
    (UNSAT
     (unsat_core
      (((literals (-90 -91 -88 -89 -87 -86)) (is_theory false))
       ((literals (-112 -111 -106 -107 -110 -108)) (is_theory false))
       ((literals (-97 -98 -92 -95 -96 -94)) (is_theory false)))))
    |}]
;;

let%expect_test "assumptions" =
  let solver = Solver.create () in
  ignore
    (Solver.add_clause solver ~clause:[| 3; -5; 6 |] : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| -2; -5; -3; 6; -4 |]
     : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| -5; 1; 4; -6 |]
     : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| 3; -4; 6; 1; 2; 4 |]
     : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| -3; 4; -2; 6; -1; -5 |]
     : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| 3; -2; -6; 4 |]
     : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| 3; 2; -1 |] : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| -6; -4; 5; -3 |]
     : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| -3; 2; 5; 6; -1; -4 |]
     : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| 4; -2; -3; 5 |]
     : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| 3; -2; -1; -5; -6; -4 |]
     : [ `Ok | `Unsat of _ ]);
  ignore (Solver.add_clause solver ~clause:[| -2; -6 |] : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| -1; -2; 4; 5 |]
     : [ `Ok | `Unsat of _ ]);
  ignore
    (Solver.add_clause solver ~clause:[| 2; -4; 1; 3; -5; -6 |]
     : [ `Ok | `Unsat of _ ]);
  let solve ?assumptions () =
    let result = Solver.solve ?assumptions solver in
    match result with
    | Sat { assignments } ->
      print_s
        [%message
          "SAT" ~assignments:(assignments_to_int_array assignments : int array)]
    | Unsat { proof } ->
      print_s
        [%message "UNSAT" ~unsat_core:(proof : Sat_result.Proof_clause.t list)]
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
    (SAT (assignments (1 2 3 4 -5 -6)))
    (UNSAT (unsat_core ()))
    (SAT (assignments (-1 -2 -3 4 -5 6)))
    (UNSAT (unsat_core (((literals (-6 -2)) (is_theory false)))))
    (SAT (assignments (-1 -2 -3 -4 -5 -6)))
    (SAT (assignments (-1 -2 -3 -4 -5 6)))
    |}]
;;
