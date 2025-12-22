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

let hard_instance ~num_vars =
  let generator = Sat_generators.hard_3_sat ~num_vars in
  let clauses = Quickcheck.random_value generator in
  let num_clauses = Array.length clauses in
  Instance.
    { name = sprintf "Hard 3-SAT (n=%d, m=%d)" num_vars num_clauses; clauses }
;;

let easy_planted_instance ~num_vars ~num_clauses =
  let generator = Sat_generators.easy_planted_3_sat ~num_vars ~num_clauses in
  let clauses, _solution = Quickcheck.random_value generator in
  Instance.
    { name = sprintf "Planted 3-SAT (n=%d, m=%d)" num_vars num_clauses
    ; clauses
    }
;;

let scale_free_instance ~num_vars ~num_clauses ~k ~alpha =
  let generator =
    Sat_generators.scale_free_k_sat ~num_vars ~num_clauses ~k ~alpha
  in
  let clauses = Quickcheck.random_value generator in
  Instance.
    { name =
        sprintf
          "Scale-free %d-SAT (n=%d, m=%d, alpha=%.2f)"
          k
          num_vars
          num_clauses
          alpha
    ; clauses
    }
;;

let uniform_instance ~num_vars ~num_clauses ~k =
  let generator =
    Sat_generators.uniform_random_k_sat ~num_vars ~num_clauses ~k
  in
  let clauses = Quickcheck.random_value generator in
  Instance.
    { name = sprintf "Uniform %d-SAT (n=%d, m=%d)" k num_vars num_clauses
    ; clauses
    }
;;

let planted_solution_instance ~num_vars ~num_clauses ~k ~min_satisfied =
  let generator =
    Sat_generators.planted_solution ~num_vars ~num_clauses ~k ~min_satisfied
  in
  let clauses, _solution = Quickcheck.random_value generator in
  Instance.
    { name =
        sprintf
          "Planted %d-SAT (n=%d, m=%d, min_sat=%d)"
          k
          num_vars
          num_clauses
          min_satisfied
    ; clauses
    }
;;

let forced_backbone_instance ~num_vars ~num_clauses ~k ~backbone_size =
  let generator =
    Sat_generators.forced_backbone ~num_vars ~num_clauses ~k ~backbone_size
  in
  let clauses, _backbone = Quickcheck.random_value generator in
  Instance.
    { name =
        sprintf
          "Forced-backbone %d-SAT (n=%d, m=%d, backbone=%d)"
          k
          num_vars
          num_clauses
          backbone_size
    ; clauses
    }
;;

let random_k_cnf_instance ~num_vars ~num_clauses ~max_k =
  let generator = Sat_generators.random_k_cnf ~num_vars ~num_clauses ~max_k in
  let clauses = Quickcheck.random_value generator in
  Instance.
    { name =
        sprintf "Random k-CNF (n=%d, m=%d, max_k=%d)" num_vars num_clauses max_k
    ; clauses
    }
;;

let compute_instance_sizes ~max_num_vars =
  let max_num_vars = Int.max max_num_vars 10 in
  let half = Int.max 50 (max_num_vars / 2) in
  [ half; max_num_vars ]
  |> List.filter ~f:(fun n -> n > 0)
  |> List.dedup_and_sort ~compare:Int.compare
;;

let default_benchmark_config =
  Benchmark.Config.create
    ~warmup_runs:1
    ~sample_runs:5
    ~target_time_ns:1_000_000_000
    ()
;;

let run_scaling_benchmark
  ?(benchmark_config = default_benchmark_config)
  ~max_num_vars
  ()
  =
  let sizes = compute_instance_sizes ~max_num_vars in
  let hard_instances = List.map sizes ~f:(fun n -> hard_instance ~num_vars:n) in
  let uniform_instances =
    List.map sizes ~f:(fun n ->
      let num_clauses =
        Int.max (4 * n) (Int.of_float (4.2 *. Float.of_int n))
      in
      uniform_instance ~num_vars:n ~num_clauses ~k:3)
  in
  let easy_planted_instances =
    List.map sizes ~f:(fun n ->
      let num_clauses =
        Int.max (3 * n) (Int.of_float (3.5 *. Float.of_int n))
      in
      easy_planted_instance ~num_vars:n ~num_clauses)
  in
  let scale_free_instances =
    List.map sizes ~f:(fun n ->
      let num_clauses =
        Int.max (3 * n) (Int.of_float (3.7 *. Float.of_int n))
      in
      scale_free_instance ~num_vars:n ~num_clauses ~k:3 ~alpha:2.3)
  in
  let planted_solution_instances =
    List.map sizes ~f:(fun n ->
      let num_clauses =
        Int.max (4 * n) (Int.of_float (4.5 *. Float.of_int n))
      in
      planted_solution_instance ~num_vars:n ~num_clauses ~k:4 ~min_satisfied:1)
  in
  let forced_backbone_instances =
    List.map sizes ~f:(fun n ->
      let num_clauses =
        Int.max (3 * n) (Int.of_float (3.8 *. Float.of_int n))
      in
      let backbone_size = Int.max 1 (n / 8) in
      forced_backbone_instance ~num_vars:n ~num_clauses ~k:3 ~backbone_size)
  in
  let random_k_cnf_instances =
    List.map sizes ~f:(fun n ->
      let num_clauses =
        Int.max (4 * n) (Int.of_float (4.0 *. Float.of_int n))
      in
      random_k_cnf_instance ~num_vars:n ~num_clauses ~max_k:5)
  in
  let benchmarks =
    List.concat
      [ hard_instances
      ; uniform_instances
      ; easy_planted_instances
      ; scale_free_instances
      ; planted_solution_instances
      ; forced_backbone_instances
      ; random_k_cnf_instances
      ]
    |> List.map ~f:benchmark_of_instance
  in
  Benchmark.run_all_and_print ~config:benchmark_config benchmarks
;;
