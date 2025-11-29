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
  (* let formula = Large_problem.formula in *)
  let formula =
    [%of_sexp: int array array]
      (Sexp.of_string
         {|
       ((-6 -3 10) (2 1 4) (7 -4 9) (9 8 5) (7 2 -4) (9 -4 6) (-6 9 8)
       (5 -1 -6) (2 -1 8) (2 3 -7) (9 -1 -4) (9 10 5) (-3 -10 -7) (10 -1 5)
       (-3 1 -4) (2 -9 4) (-6 -2 -3) (10 7 8) (2 1 9) (-8 1 9) (4 -7 -5)
       (-6 -4 -1) (7 6 3) (-8 7 -5) (7 -9 10))
|})
  in
  Array.iter formula ~f:(fun clause ->
    ignore (Solver.add_clause' solver ~clause));
  let result = Solver.solve solver in
  match result with
  | Sat { assignments } ->
    print_s [%message "SAT" (Clause.to_int_array assignments : int array)]
  | Unsat { unsat_core } ->
    print_s [%message "UNSAT" (Clause.to_int_array unsat_core : int array)]
;;
