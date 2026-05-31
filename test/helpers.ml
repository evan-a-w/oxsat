open! Core
open! Feel

let assignment_literals assignments =
  Array.filter_mapi assignments ~f:(fun var assignment ->
    match assignment with
    | None -> None
    | Some true -> Some var
    | Some false -> Some (-var))
;;

let add_clause_exn solver ~clause =
  match Solver.add_clause solver ~clause with
  | `Ok -> ()
  | `Unsat unsat_core ->
    Error.raise_s
      [%message
        "unexpected immediate UNSAT"
          (clause : int array)
          (unsat_core : int array)]
;;

let solve_formula ?debug ?(time_bound = `Bounded 1_000) ?assumptions formula =
  match Solver.create_with_formula ?debug formula with
  | `Unsat unsat_core -> Sat_result.Unsat { unsat_core }
  | `Ok solver -> Solver.solve ~time_bound ?assumptions solver
;;
