open! Core

let run_examples ?min_iterations ?max_iterations ?sample_runs ~only () =
  print_endline "=== Simple Benchmark Examples ===\n";
  let results = ref [] in
  let run ~name f =
    if Benchmark.matches_only ~only name
    then (
      let result = Benchmark.run_and_print ~name f in
      results := result :: !results)
  in
  (* Example 1: Single function benchmark *)
  print_endline "Benchmark: List.range allocation";
  run ~name:"List.range 0 100" (fun () -> List.range 0 100);
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
  let more_results =
    Benchmark.run_all_and_print
      ~config
      ~only
      [ ("List.range 0 100", fun () -> List.range 0 100)
      ; ("List.init 100", fun () -> List.init 100 ~f:Fn.id)
      ; ("List.range 0 1000", fun () -> List.range 0 1000)
      ; ("List.range 0 10000", fun () -> List.range 0 10000)
      ]
  in
  results := more_results @ !results;
  print_endline "";
  (* Example 3: More intensive computation *)
  print_endline "Benchmark: Fibonacci computation";
  let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2) in
  run ~name:"fib 20" (fun () -> fib 20);
  List.rev !results
;;

let run_rb ?min_iterations ?max_iterations ?sample_runs ~only () =
  print_endline "RB tree scaling benchmarks:";
  let results =
    Rb_bench.run_scaling_benchmark
      ~benchmark_config:
        (Benchmark.Config.create
           ?min_iterations
           ?max_iterations
           ~sample_runs:(Option.value sample_runs ~default:10)
           ())
      ~tree_sizes:[ 10; 100; 1000; 10_000; 100_000; 1_000_000; 10_000_000 ]
      ~num_operations:1000
      ~only
      ()
  in
  Benchmark.print_results results;
  results
;;

let run_map ?min_iterations ?max_iterations ?sample_runs ~only () =
  print_endline "Core Map scaling benchmarks:";
  let results =
    Map_bench.run_scaling_benchmark
      ~benchmark_config:
        (Benchmark.Config.create
           ?min_iterations
           ?max_iterations
           ~sample_runs:(Option.value sample_runs ~default:10)
           ())
      ~tree_sizes:[ 10; 100; 1000; 10_000; 100_000; 1_000_000; 10_000_000 ]
      ~num_operations:1000
      ~only
      ()
  in
  Benchmark.print_results results;
  results
;;

let run_sat ?min_iterations ?max_iterations ?sample_runs ~only ~max_num_vars () =
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
  Sat_bench.run_scaling_benchmark ~benchmark_config ~only ~max_num_vars ()
;;

let run_dimacs ?min_iterations ?max_iterations ?sample_runs ~only () =
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
  Dimacs_bench.run_dimacs_examples ~benchmark_config ~only ()
;;

let run_dimacs_sat_js ?min_iterations ?max_iterations ?sample_runs ~only () =
  print_endline "DIMACS example benchmarks with sat.js:";
  let benchmark_config =
    Benchmark.Config.create
      ?min_iterations
      ?max_iterations
      ~warmup_runs:1
      ~sample_runs:(Option.value sample_runs ~default:5)
      ~target_time_ns:1_000_000_000
      ()
  in
  Sat_js_dimacs_bench.run_dimacs_examples ~benchmark_config ~only ()
;;

let run_smt ?min_iterations ?max_iterations ?sample_runs ~only () =
  print_endline "SMT solver benchmarks (LP, MILP, EUF):";
  let benchmark_config =
    Benchmark.Config.create
      ?min_iterations
      ?max_iterations
      ~warmup_runs:1
      ~sample_runs:(Option.value sample_runs ~default:3)
      ~target_time_ns:2_000_000_000
      ()
  in
  Smt_bench.run_scaling_benchmark ~benchmark_config ~only ()
;;

let run_dimacs_sat_js_ocaml
  ?min_iterations
  ?max_iterations
  ?sample_runs
  ~only
  ()
  =
  print_endline "DIMACS example benchmarks with OCaml sat.js equivalent:";
  let benchmark_config =
    Benchmark.Config.create
      ?min_iterations
      ?max_iterations
      ~warmup_runs:1
      ~sample_runs:(Option.value sample_runs ~default:5)
      ~target_time_ns:1_000_000_000
      ()
  in
  Simple_sat_dimacs_bench.run_dimacs_examples ~benchmark_config ~only ()
;;

let command =
  Command.basic
    ~summary:"Run benchmarks, or compare two saved result files"
    (let open Command.Let_syntax in
     let%map_open bench =
       flag
         "bench"
         (optional string)
         ~doc:
           "BENCH Which benchmark to run (examples, rb, map, sat, dimacs, \
            dimacs-sat-js, dimacs-sat-js-ocaml, smt, or all). Default: all"
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
     and only =
       flag
         "only"
         (listed string)
         ~doc:
           "SUBSTRING Only run benchmarks whose name contains SUBSTRING. May \
            be passed multiple times to match any of several substrings. In \
            compare mode, filters both files by name."
     and output =
       flag
         "o"
         (optional string)
         ~doc:"FILE Save results to FILE as sexp, for later comparison"
     and compare_files =
       anon (maybe (t2 ("before" %: string) ("after" %: string)))
     in
     fun () ->
       match compare_files with
       | Some (before, after) ->
         let load filename =
           Benchmark.load_results ~filename
           |> List.filter ~f:(fun (r : Benchmark.Result.t) ->
             Benchmark.matches_only ~only r.name)
         in
         Benchmark.compare_results ~before:(load before) ~after:(load after)
       | None ->
         let sat_max_n = Option.value sat_max_n ~default:400 in
         let run f = f ?min_iterations ?max_iterations ?sample_runs ~only in
         let results =
           match bench with
           | None | Some "all" ->
             List.concat
               [ run run_examples ()
               ; run run_rb ()
               ; run run_map ()
               ; run run_sat ~max_num_vars:sat_max_n ()
               ; run run_dimacs ()
               ; run run_smt ()
               ]
           | Some "examples" -> run run_examples ()
           | Some "rb" -> run run_rb ()
           | Some "map" -> run run_map ()
           | Some "sat" -> run run_sat ~max_num_vars:sat_max_n ()
           | Some "dimacs" -> run run_dimacs ()
           | Some "dimacs-sat-js" -> run run_dimacs_sat_js ()
           | Some "dimacs-sat-js-ocaml" -> run run_dimacs_sat_js_ocaml ()
           | Some "smt" -> run run_smt ()
           | Some other ->
             eprintf "Unknown benchmark: %s\n" other;
             eprintf
               "Valid options: examples, rb, map, sat, dimacs, dimacs-sat-js, \
                dimacs-sat-js-ocaml, smt, all\n";
             exit 1
         in
         (match output with
          | Some filename -> Benchmark.save_results ~filename results
          | None -> ()))
;;

let () = Memtrace_hook.wrap (fun () -> Command_unix.run command)
