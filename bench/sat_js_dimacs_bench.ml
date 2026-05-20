open! Core

module Prepared_instance = struct
  type t =
    { name : string
    ; path : string
    }
end

let runner_path = "bench/sat_js_dimacs_runner.js"
let solver_path = "sat.js"

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

let command_for_instance (instance : Prepared_instance.t) =
  String.concat
    ~sep:" "
    [ "node"
    ; Filename.quote runner_path
    ; "--solver"
    ; Filename.quote solver_path
    ; "--dimacs"
    ; Filename.quote instance.path
    ]
;;

let solve_instance instance =
  match Core_unix.system (command_for_instance instance) with
  | Ok () -> ()
  | Error error ->
    Error.raise_s
      [%message
        "sat.js failed"
          (instance.name : string)
          ~error:(Core_unix.Exit_or_signal.to_string_hum (Error error) : string)]
;;

let benchmark_of_instance instance =
  let run () = solve_instance instance in
  instance.name, run
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
  let benchmarks = List.map instances ~f:benchmark_of_instance in
  Benchmark.run_all_and_print ~config:benchmark_config benchmarks
;;
