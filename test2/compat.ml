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

  type t = Feel2.Solver.t

  module Stats = Feel2.Stats

  let create = Feel2.Solver.create
  let create_with_formula = Feel2.Solver.create_with_formula

  let add_clause' t ~clause =
    ignore (Feel2.Solver.add_clause t ~clause);
    t
  ;;

  let add_clause t ~clause = Feel2.Solver.add_clause t ~clause
  let stats = Feel2.Solver.stats

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

  let solve ?assumptions t =
    match Feel2.Solver.solve ?assumptions t with
    | Feel2.Sat_result.Sat { assignments } ->
      Sat_result.Sat { assignments = assignment_clause assignments }
    | Unsat { unsat_core } ->
      Sat_result.Unsat { unsat_core = Clause.of_int_array unsat_core }
  ;;
end
