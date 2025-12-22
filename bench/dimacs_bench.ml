open! Core
open! Feel

module Instance = struct
  type t =
    { name : string
    ; clauses : int array array
    }
end

let solve_instance clauses =
  let solver = Solver.create () in
  Array.iter clauses ~f:(fun clause ->
    ignore (Solver.add_clause' solver ~clause : Solver.t));
  match Solver.solve solver with
  | Sat _ | Unsat _ -> ()
;;

let benchmark_of_instance (instance : Instance.t) =
  let run () = solve_instance instance.clauses in
  instance.name, run
;;

let dimacs_instance ~name dimacs_string =
  let clauses =
    Examples.Dimacs.read_string dimacs_string
    |> List.map ~f:(fun clause -> Array.of_list clause)
    |> Array.of_list
  in
  Instance.{ name; clauses }
;;

let default_benchmark_config =
  Benchmark.Config.create
    ~warmup_runs:1
    ~sample_runs:5
    ~target_time_ns:1_000_000_000
    ()
;;

let run_dimacs_examples ?(benchmark_config = default_benchmark_config) () =
  let instances =
    [ dimacs_instance ~name:"SUDOKU" Examples.Dimacs.sudoku
    ; dimacs_instance ~name:"SUCC_EG" Examples.Dimacs.succ_eg
    ; dimacs_instance ~name:"FACTOR_1234321" Examples.Dimacs.factor_1234321
    ; dimacs_instance ~name:"FACTOR_1235321" Examples.Dimacs.factor_1235321
    ; dimacs_instance ~name:"FAIL_EG" Examples.Dimacs.fail_eg
    ; dimacs_instance ~name:"SUBSETS_100" Examples.Dimacs.subsets_100
    ]
  in
  let benchmarks = List.map instances ~f:benchmark_of_instance in
  Benchmark.run_all_and_print ~config:benchmark_config benchmarks
;;
