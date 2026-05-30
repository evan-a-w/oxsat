open! Core
open! Stdlib_stable
module Clause = Feel.Clause
module Sat_generators = Feel.Sat_generators

module Solver = struct
  module Sat_result = struct
    type t =
      | Sat of { assignments : Clause.t }
      | Unsat of { unsat_core : Clause.t }
    [@@deriving sexp]
  end

  type t =
    { solver : Feel2.Solver.t
    ; mutable pending_unsat : Clause.t option
    }

  module Stats = Feel2.Stats

  exception Timeout = Feel2.Solver.Timeout

  let create ?random_state ?debug () =
    { solver = Feel2.Solver.create ?random_state ?debug ()
    ; pending_unsat = None
    }
  ;;

  let stash_add_clause_result t = function
    | `Ok -> ()
    | `Unsat unsat_core ->
      t.pending_unsat <- Some (Clause.of_int_array unsat_core)
  ;;

  let add_clause t ~clause =
    match t.pending_unsat with
    | Some _ -> ()
    | None ->
      stash_add_clause_result t (Feel2.Solver.add_clause t.solver ~clause)
  ;;

  let add_clause' t ~clause =
    add_clause t ~clause;
    t
  ;;

  let create_with_formula ?debug formula =
    let t = create ?debug () in
    Array.iter formula ~f:(fun clause -> add_clause t ~clause);
    t
  ;;

  let stats t = Feel2.Solver.stats t.solver

  let assignment_clause assignments =
    let literals = ref [] in
    for var = 1 to Array.length assignments - 1 do
      match assignments.(var) with
      | None -> ()
      | Some true -> literals := var :: !literals
      | Some false -> literals := -var :: !literals
    done;
    Clause.of_int_array (Array.of_list_rev !literals)
  ;;

  let solve ?time_bound ?assumptions t =
    match t.pending_unsat with
    | Some unsat_core -> Sat_result.Unsat { unsat_core }
    | None ->
      (match Feel2.Solver.solve ?time_bound ?assumptions t.solver with
       | Feel2.Sat_result.Sat { assignments } ->
         Sat_result.Sat { assignments = assignment_clause assignments }
       | Unsat { unsat_core } ->
         Sat_result.Unsat { unsat_core = Clause.of_int_array unsat_core })
  ;;
end
