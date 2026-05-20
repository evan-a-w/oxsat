open! Core

module Prepared_instance = struct
  type t =
    { name : string
    ; path : string
    }
end

let runner_path = "bench/sat_js_dimacs_runner.js"
let solver_path = "bench/sat.js"

let prepare_instance (instance : Dimacs_bench.Instance.t) =
  let path =
    Stdlib.Filename.temp_file
      ~temp_dir:(Stdlib.Filename.get_temp_dir_name ())
      (String.lowercase instance.name ^ "-")
      ".cnf"
  in
  Out_channel.write_all path ~data:instance.dimacs;
  Prepared_instance.{ name = instance.name; path }
;;

let command_for_instance
  ~(benchmark_config : Benchmark.Config.t)
  (instance : Prepared_instance.t)
  =
  String.concat
    ~sep:" "
    [ "node"
    ; Filename.quote runner_path
    ; "--solver"
    ; Filename.quote solver_path
    ; "--dimacs"
    ; Filename.quote instance.path
    ; "--name"
    ; Filename.quote instance.name
    ; "--benchmark"
    ; "--warmup-runs"
    ; Int.to_string benchmark_config.warmup_runs
    ; "--sample-runs"
    ; Int.to_string benchmark_config.sample_runs
    ; "--min-iterations"
    ; Int.to_string benchmark_config.min_iterations
    ; "--max-iterations"
    ; Int.to_string benchmark_config.max_iterations
    ; "--target-time-ns"
    ; Int.to_string benchmark_config.target_time_ns
    ]
;;

let benchmark_instance ~benchmark_config (instance : Prepared_instance.t) =
  match Core_unix.system (command_for_instance ~benchmark_config instance) with
  | Ok () -> ()
  | Error error ->
    Error.raise_s
      [%message
        "sat.js failed"
          (instance.name : string)
          ~error:(Core_unix.Exit_or_signal.to_string_hum (Error error) : string)]
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
    Dimacs_bench.default_instances () |> List.map ~f:prepare_instance
  in
  List.iter instances ~f:(benchmark_instance ~benchmark_config);
  []
;;
