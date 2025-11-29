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
  let formula : int array array =
    [%of_sexp: int array array]
      (Sexp.of_string
         {|
((-1 -4 -3) (-1 -5 2) (-8 1 -2) (-6 1 3) (-1 -4 -3) (1 -3 2) (-7 1 2)
      (-1 4 -2) (1 -3 2) (1 3 2) (-1 3 5) (-1 -4 2) (-6 1 -2) (1 -4 3)
      (8 -1 -4) (-1 4 -2) (8 1 2) (1 -4 -2) (-1 -4 3) (-1 -3 -2) (-1 3 -5)
      (-1 3 -2) (1 -4 -2) (-1 -5 -2) (-1 -3 2) (1 3 2) (1 -4 -2) (-6 1 2)
      (-1 -5 -2) (-1 3 2))
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
