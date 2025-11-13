open! Core
open! Feel
open! Ds

let () =
  let solver = Solver.create ~debug:true () in
  (* Add clauses: x1 and x2 *)
  (* let _ = *)
  (*   solver *)
  (*   |> Solver.add_clause' ~clause:[| 1; 2 |] *)
  (*   |> Solver.add_clause' ~clause:[| 1; -2 |] *)
  (*   |> Solver.add_clause' ~clause:[| -1; 3 |] *)
  (*   |> Solver.add_clause' ~clause:[| -1; -3 |] *)
  (* in *)
  Array.iter Large_problem.formula ~f:(fun clause ->
      ignore (Solver.add_clause' solver ~clause));
  let result = Solver.solve solver in
  match result with
  | Sat { assignments } ->
    print_s [%message "SAT" (Clause.to_int_array assignments : int array)]
  | Unsat { unsat_core } ->
    print_s [%message "UNSAT" (Clause.to_int_array unsat_core : int array)]
;;
