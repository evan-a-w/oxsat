open! Core
open! Feel
open! Ds

[@@@warning "-69"]

(** Example tests demonstrating the SAT generators *)

let%expect_test "uniform random 3-SAT small instance" =
  let generator = Sat_generators.uniform_random_k_sat ~num_vars:5 ~num_clauses:10 ~k:3 in
  let clauses = Quickcheck.random_value generator in
  print_s [%message
    "Generated uniform random 3-SAT"
      (Array.length clauses : int)
      (clauses.(0) : int array)];
  [%expect {|
    ("Generated uniform random 3-SAT" ("Array.length clauses" 10)
     ("clauses.(0)" (3 1 -2)))
    |}]
;;

let%expect_test "hard 3-SAT near phase transition" =
  let generator = Sat_generators.hard_3_sat ~num_vars:10 in
  let clauses = Quickcheck.random_value generator in
  (* Should generate ~42-43 clauses (4.26 * 10) *)
  let num_clauses = Array.length clauses in
  print_s [%message
    "Generated hard 3-SAT"
      (num_clauses : int)];
  assert (num_clauses >= 40 && num_clauses <= 45);
  [%expect {| ("Generated hard 3-SAT" (num_clauses 42)) |}]
;;

let%expect_test "planted solution is actually satisfiable" =
  let generator = Sat_generators.easy_planted_3_sat ~num_vars:10 ~num_clauses:20 in
  let clauses, solution = Quickcheck.random_value generator in

  (* Verify the planted solution actually satisfies all clauses *)
  let all_satisfied =
    Array.for_all clauses ~f:(fun clause ->
      Array.exists clause ~f:(fun lit ->
        let var = Int.abs lit - 1 in
        let expected_value = lit > 0 in
        Bool.equal solution.(var) expected_value))
  in

  print_s [%message
    "Planted solution satisfies all clauses"
      (all_satisfied : bool)
      (Array.length clauses : int)];
  [%expect {|
    ("Planted solution satisfies all clauses" (all_satisfied true)
     ("Array.length clauses" 20))
    |}]
;;

let%expect_test "scale-free distribution favors some variables" =
  let generator = Sat_generators.scale_free_k_sat ~num_vars:20 ~num_clauses:50 ~k:3 ~alpha:2.5 in
  let clauses = Quickcheck.random_value generator in

  (* Count variable frequencies *)
  let frequencies = Array.create ~len:20 0 in
  Array.iter clauses ~f:(fun clause ->
    Array.iter clause ~f:(fun lit ->
      let var = Int.abs lit - 1 in
      frequencies.(var) <- frequencies.(var) + 1));

  (* Variables 1-3 should appear more frequently than variables 18-20 *)
  let low_vars_freq = frequencies.(0) + frequencies.(1) + frequencies.(2) in
  let high_vars_freq = frequencies.(17) + frequencies.(18) + frequencies.(19) in

  print_s [%message
    "Scale-free: low-numbered vars appear more"
      (low_vars_freq : int)
      (high_vars_freq : int)
      (low_vars_freq > high_vars_freq : bool)];
  [%expect {|
    ("Scale-free: low-numbered vars appear more" (low_vars_freq 111)
     (high_vars_freq 1) ("low_vars_freq > high_vars_freq" true))
    |}]
;;

let%expect_test "solve simple planted 3-SAT instance" =
  let generator = Sat_generators.easy_planted_3_sat ~num_vars:5 ~num_clauses:10 in
  let clauses, _solution = Quickcheck.random_value generator in

  let solver = Solver.create () in
  Array.iter clauses ~f:(fun clause ->
    ignore (Solver.add_clause' solver ~clause : Solver.t));

  let result = Solver.solve solver in
  (match result with
   | Sat _ -> print_endline "SAT (as expected for planted solution)"
   | Unsat _ -> print_endline "UNSAT (unexpected!)");
  [%expect {| SAT (as expected for planted solution) |}]
;;

let%expect_test "forced backbone constraints" =
  let generator = Sat_generators.forced_backbone ~num_vars:10 ~num_clauses:20 ~k:3 ~backbone_size:3 in
  let clauses, backbone = Quickcheck.random_value generator in

  print_s [%message
    "Generated instance with backbone"
      (Array.length clauses : int)
      (List.length backbone : int)];

  (* First few clauses should be unit clauses forcing the backbone *)
  let backbone_size = List.length backbone in
  let unit_clauses =
    Array.sub clauses ~pos:0 ~len:backbone_size
    |> Array.for_all ~f:(fun clause -> Array.length clause = 1)
  in

  print_s [%message
    "Backbone enforced with unit clauses"
      (unit_clauses : bool)];
  [%expect {|
    ("Generated instance with backbone" ("Array.length clauses" 23)
     ("List.length backbone" 3))
    ("Backbone enforced with unit clauses" (unit_clauses true))
    |}]
;;

let%test_unit "uniform k-SAT generates correct clause lengths" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int array array]
    (Sat_generators.uniform_random_k_sat ~num_vars:10 ~num_clauses:20 ~k:3)
    ~f:(fun clauses ->
      assert (Array.length clauses = 20);
      Array.iter clauses ~f:(fun clause ->
        assert (Array.length clause = 3)))
;;

let%test_unit "planted solutions are always satisfiable" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int array array * bool array]
    (Sat_generators.planted_solution ~num_vars:15 ~num_clauses:30 ~k:3 ~min_satisfied:1)
    ~f:(fun (clauses, solution) ->
      (* Check that the planted solution satisfies every clause *)
      Array.iter clauses ~f:(fun clause ->
        let satisfied =
          Array.exists clause ~f:(fun lit ->
            let var = Int.abs lit - 1 in
            let expected_value = lit > 0 in
            Bool.equal solution.(var) expected_value)
        in
        assert satisfied))
;;

let%test_unit "k-CNF generates variable length clauses" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int array array]
    (Sat_generators.random_k_cnf ~num_vars:10 ~num_clauses:30 ~max_k:4)
    ~f:(fun clauses ->
      assert (Array.length clauses = 30);
      Array.iter clauses ~f:(fun clause ->
        assert (Array.length clause >= 1 && Array.length clause <= 4)))
;;

(** Helper function to verify that a solution satisfies all clauses *)
let verify_solution ~clauses ~assignments =
  Array.for_all clauses ~f:(fun clause ->
    Array.exists clause ~f:(fun lit ->
      let var = Int.abs lit in
      let expected_value = lit > 0 in
      (* Find the literal in assignments *)
      Array.exists assignments ~f:(fun assigned_lit ->
        let assigned_var = Int.abs assigned_lit in
        let assigned_value = assigned_lit > 0 in
        assigned_var = var && Bool.equal assigned_value expected_value)))
;;

(** QuickCheck tests that actually test the SAT solver *)

let%test_unit "solver finds solutions for planted SAT instances" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int array array * bool array]
    ~trials:50
    (Sat_generators.planted_solution ~num_vars:10 ~num_clauses:25 ~k:3 ~min_satisfied:1)
    ~f:(fun (clauses, _planted_solution) ->
      let solver = Solver.create () in
      Array.iter clauses ~f:(fun clause ->
        ignore (Solver.add_clause' solver ~clause : Solver.t));

      match Solver.solve solver with
      | Sat { assignments } ->
        (* Verify the solution actually satisfies all clauses *)
        let assignments_array = Clause.to_int_array assignments in
        assert (verify_solution ~clauses ~assignments:assignments_array)
      | Unsat _ ->
        (* Planted solutions should always be satisfiable *)
        failwith "Solver returned UNSAT for planted SAT instance")
;;

let%test_unit "solver solutions are valid for uniform random 3-SAT" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int array array]
    ~trials:30
    (Sat_generators.uniform_random_k_sat ~num_vars:8 ~num_clauses:15 ~k:3)
    ~f:(fun clauses ->
      let solver = Solver.create () in
      Array.iter clauses ~f:(fun clause ->
        ignore (Solver.add_clause' solver ~clause : Solver.t));

      match Solver.solve solver with
      | Sat { assignments } ->
        (* If solver claims SAT, verify the solution is correct *)
        let assignments_array = Clause.to_int_array assignments in
        assert (verify_solution ~clauses ~assignments:assignments_array)
      | Unsat _ ->
        (* If solver claims UNSAT, we can't easily verify, but at least it terminated *)
        ())
;;

let%test_unit "solver handles scale-free instances" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int array array]
    ~trials:20
    (Sat_generators.scale_free_k_sat ~num_vars:12 ~num_clauses:30 ~k:3 ~alpha:2.5)
    ~f:(fun clauses ->
      let solver = Solver.create () in
      Array.iter clauses ~f:(fun clause ->
        ignore (Solver.add_clause' solver ~clause : Solver.t));

      match Solver.solve solver with
      | Sat { assignments } ->
        let assignments_array = Clause.to_int_array assignments in
        assert (verify_solution ~clauses ~assignments:assignments_array)
      | Unsat _ -> ())
;;

let%test_unit "solver respects backbone constraints" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int array array * (int * bool) list]
    ~trials:30
    (Sat_generators.forced_backbone ~num_vars:10 ~num_clauses:20 ~k:3 ~backbone_size:3)
    ~f:(fun (clauses, backbone) ->
      let solver = Solver.create () in
      Array.iter clauses ~f:(fun clause ->
        ignore (Solver.add_clause' solver ~clause : Solver.t));

      match Solver.solve solver with
      | Sat { assignments } ->
        let assignments_array = Clause.to_int_array assignments in
        (* Verify solution satisfies all clauses *)
        assert (verify_solution ~clauses ~assignments:assignments_array);

        (* Verify backbone constraints are respected *)
        List.iter backbone ~f:(fun (var, expected_value) ->
          let var_assignment =
            Array.find_map assignments_array ~f:(fun lit ->
              if Int.abs lit = var
              then Some (lit > 0)
              else None)
          in
          match var_assignment with
          | Some actual_value ->
            if not (Bool.equal actual_value expected_value)
            then failwithf "Backbone constraint violated: var %d should be %b but is %b"
                   var expected_value actual_value ()
          | None ->
            failwithf "Backbone variable %d not assigned" var ())
      | Unsat _ ->
        (* Backbone instances might be UNSAT if constraints conflict *)
        ())
;;

let%test_unit "solver handles variable length clauses (k-CNF)" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int array array]
    ~trials:30
    (Sat_generators.random_k_cnf ~num_vars:10 ~num_clauses:25 ~max_k:4)
    ~f:(fun clauses ->
      let solver = Solver.create () in
      Array.iter clauses ~f:(fun clause ->
        ignore (Solver.add_clause' solver ~clause : Solver.t));

      match Solver.solve solver with
      | Sat { assignments } ->
        let assignments_array = Clause.to_int_array assignments in
        assert (verify_solution ~clauses ~assignments:assignments_array)
      | Unsat _ -> ())
;;

let%test_unit "solver handles easy SAT instances quickly" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int array array * bool array]
    ~trials:50
    (Sat_generators.easy_planted_3_sat ~num_vars:15 ~num_clauses:40)
    ~f:(fun (clauses, _solution) ->
      let solver = Solver.create () in
      Array.iter clauses ~f:(fun clause ->
        ignore (Solver.add_clause' solver ~clause : Solver.t));

      (* These should all be SAT since they're planted *)
      match Solver.solve solver with
      | Sat { assignments } ->
        let assignments_array = Clause.to_int_array assignments in
        assert (verify_solution ~clauses ~assignments:assignments_array)
      | Unsat _ ->
        failwith "Solver returned UNSAT for easy planted instance")
;;

let%test_unit "solver determinism: same instance gives same result" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int array array]
    ~trials:20
    (Sat_generators.uniform_random_k_sat ~num_vars:8 ~num_clauses:20 ~k:3)
    ~f:(fun clauses ->
      (* Solve the same instance twice *)
      let solve_instance () =
        let solver = Solver.create () in
        Array.iter clauses ~f:(fun clause ->
          ignore (Solver.add_clause' solver ~clause : Solver.t));
        Solver.solve solver
      in

      let result1 = solve_instance () in
      let result2 = solve_instance () in

      (* Both results should be the same (SAT or UNSAT) *)
      match result1, result2 with
      | Sat _, Sat _ -> ()
      | Unsat _, Unsat _ -> ()
      | _ -> failwith "Solver gave different results for same instance")
;;

let%test_unit "solver handles contradictory clauses correctly" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int]
    ~trials:20
    (Int.gen_incl 1 10)
    ~f:(fun var ->
      (* Create a contradiction: x and -x *)
      let solver = Solver.create () in
      ignore (Solver.add_clause' solver ~clause:[| var |] : Solver.t);
      ignore (Solver.add_clause' solver ~clause:[| -var |] : Solver.t);

      match Solver.solve solver with
      | Sat _ -> failwith "Solver returned SAT for contradictory clauses"
      | Unsat _ -> (* Expected *)
        ())
;;

let%test_unit "empty clause is always UNSAT" =
  let solver = Solver.create () in
  ignore (Solver.add_clause' solver ~clause:[||] : Solver.t);

  match Solver.solve solver with
  | Sat _ -> failwith "Solver returned SAT for empty clause"
  | Unsat _ -> ()
;;

let%test_unit "solver handles large planted instances" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int array array * bool array]
    ~trials:10
    (Sat_generators.planted_solution ~num_vars:30 ~num_clauses:100 ~k:3 ~min_satisfied:2)
    ~f:(fun (clauses, _solution) ->
      let solver = Solver.create () in
      Array.iter clauses ~f:(fun clause ->
        ignore (Solver.add_clause' solver ~clause : Solver.t));

      match Solver.solve solver with
      | Sat { assignments } ->
        let assignments_array = Clause.to_int_array assignments in
        assert (verify_solution ~clauses ~assignments:assignments_array)
      | Unsat _ ->
        failwith "Solver returned UNSAT for large planted instance")
;;
