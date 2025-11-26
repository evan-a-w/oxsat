open! Core

let run_examples ?min_iterations ?max_iterations ?sample_runs () =
  print_endline "=== Simple Benchmark Examples ===\n";
  (* Example 1: Single function benchmark *)
  print_endline "Benchmark: List.range allocation";
  let (_ : Benchmark.Result.t) =
    Benchmark.run_and_print ~name:"List.range 0 100" (fun () ->
      List.range 0 100)
  in
  print_endline "";
  (* Example 2: Multiple benchmarks with custom config *)
  print_endline "Multiple benchmarks with custom config:";
  let config =
    Benchmark.Config.create
      ?min_iterations
      ?max_iterations
      ~sample_runs:(Option.value sample_runs ~default:50)
      ~warmup_runs:2
      ~target_time_ns:50_000_000
      ()
  in
  let (_ : Benchmark.Result.t list) =
    Benchmark.run_all_and_print
      ~config
      [ ("List.range 0 100", fun () -> List.range 0 100)
      ; ("List.init 100", fun () -> List.init 100 ~f:Fn.id)
      ; ("List.range 0 1000", fun () -> List.range 0 1000)
      ; ("List.range 0 10000", fun () -> List.range 0 10000)
      ]
  in
  print_endline "";
  (* Example 3: More intensive computation *)
  print_endline "Benchmark: Fibonacci computation";
  let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2) in
  let (_ : Benchmark.Result.t) =
    Benchmark.run_and_print ~name:"fib 20" (fun () -> fib 20)
  in
  ()
;;

let run_rb ?min_iterations ?max_iterations ?sample_runs () =
  print_endline "RB tree scaling benchmarks:";
  let (_ : Benchmark.Result.t list) =
    Rb_bench.run_scaling_benchmark
      ~benchmark_config:
        (Benchmark.Config.create
           ?min_iterations
           ?max_iterations
           ~sample_runs:(Option.value sample_runs ~default:10)
           ())
      ~tree_sizes:[ 10; 100; 1000; 10_000; 100_000; 1_000_000; 10_000_000 ]
      ~num_operations:1000
      ()
    |> fun results ->
    Benchmark.print_results results;
    results
  in
  ()
;;

let run_map ?min_iterations ?max_iterations ?sample_runs () =
  print_endline "Core Map scaling benchmarks:";
  let (_ : Benchmark.Result.t list) =
    Map_bench.run_scaling_benchmark
      ~benchmark_config:
        (Benchmark.Config.create
           ?min_iterations
           ?max_iterations
           ~sample_runs:(Option.value sample_runs ~default:10)
           ())
      ~tree_sizes:[ 10; 100; 1000; 10_000; 100_000; 1_000_000; 10_000_000 ]
      ~num_operations:1000
      ()
    |> fun results ->
    Benchmark.print_results results;
    results
  in
  ()
;;

let run_sat ?min_iterations ?max_iterations ?sample_runs ~max_num_vars () =
  printf "SAT solver benchmarks up to n=%d variables\n" max_num_vars;
  let benchmark_config =
    Benchmark.Config.create
      ?min_iterations
      ?max_iterations
      ~warmup_runs:1
      ~sample_runs:(Option.value sample_runs ~default:5)
      ~target_time_ns:1_000_000_000
      ()
  in
  let (_ : Benchmark.Result.t list) =
    Sat_bench.run_scaling_benchmark ~benchmark_config ~max_num_vars ()
  in
  ()
;;

let run_dimacs ?min_iterations ?max_iterations ?sample_runs () =
  print_endline "DIMACS example benchmarks:";
  let benchmark_config =
    Benchmark.Config.create
      ?min_iterations
      ?max_iterations
      ~warmup_runs:1
      ~sample_runs:(Option.value sample_runs ~default:5)
      ~target_time_ns:1_000_000_000
      ()
  in
  let (_ : Benchmark.Result.t list) =
    Dimacs_bench.run_dimacs_examples ~benchmark_config ()
  in
  ()
;;

let command =
  Command.basic
    ~summary:"Run benchmarks"
    (let open Command.Let_syntax in
     let%map_open bench =
       flag
         "bench"
         (optional string)
         ~doc:
           "BENCH Which benchmark to run (examples, rb, map, sat, dimacs, or \
            all). Default: all"
     and sat_max_n =
       flag
         "sat-max-n"
         (optional int)
         ~doc:
           "INT Maximum number of variables to use for SAT benchmarks \
            (default: 400)"
     and min_iterations =
       flag
         "min-iterations"
         (optional int)
         ~doc:"INT min iterations for each test"
     and max_iterations =
       flag
         "max-iterations"
         (optional int)
         ~doc:"INT max iterations for each test"
     and sample_runs =
       flag
         "sample-runs"
         (optional int)
         ~doc:"INT number of samples to collect for each test"
     in
     fun () ->
       let sat_max_n = Option.value sat_max_n ~default:400 in
       match bench with
       | None | Some "all" ->
         run_examples ?min_iterations ?max_iterations ?sample_runs ();
         print_endline "\n";
         run_rb ?min_iterations ?max_iterations ?sample_runs ();
         print_endline "\n";
         run_map ?min_iterations ?max_iterations ?sample_runs ();
         print_endline "\n";
         run_sat
           ?min_iterations
           ?max_iterations
           ?sample_runs
           ~max_num_vars:sat_max_n
           ();
         print_endline "\n";
         run_dimacs ?min_iterations ?max_iterations ?sample_runs ()
       | Some "examples" ->
         run_examples ?min_iterations ?max_iterations ?sample_runs ()
       | Some "rb" -> run_rb ?min_iterations ?max_iterations ?sample_runs ()
       | Some "map" -> run_map ?min_iterations ?max_iterations ?sample_runs ()
       | Some "sat" ->
         run_sat
           ?min_iterations
           ?max_iterations
           ?sample_runs
           ~max_num_vars:sat_max_n
           ()
       | Some "dimacs" ->
         run_dimacs ?min_iterations ?max_iterations ?sample_runs ()
       | Some other ->
         eprintf "Unknown benchmark: %s\n" other;
         eprintf "Valid options: examples, rb, map, sat, dimacs, all\n";
         exit 1)
;;

let () = Command_unix.run command
