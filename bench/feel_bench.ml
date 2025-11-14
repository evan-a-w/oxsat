open! Core

let run_examples () =
  print_endline "=== Simple Benchmark Examples ===\n";

  (* Example 1: Single function benchmark *)
  print_endline "Benchmark: List.range allocation";
  let (_ : Benchmark.Result.t) =
    Benchmark.run_and_print ~name:"List.range 0 100" (fun () -> List.range 0 100)
  in
  print_endline "";

  (* Example 2: Multiple benchmarks with custom config *)
  print_endline "Multiple benchmarks with custom config:";
  let config =
    Benchmark.Config.create
      ~warmup_runs:2
      ~sample_runs:50
      ~target_time_ns:50_000_000
      ()
  in
  let (_ : Benchmark.Result.t list) =
    Benchmark.run_all_and_print
      ~config
      [ "List.range 0 100", (fun () -> List.range 0 100)
      ; "List.init 100", (fun () -> List.init 100 ~f:Fn.id)
      ; "List.range 0 1000", (fun () -> List.range 0 1000)
      ; "List.range 0 10000", (fun () -> List.range 0 10000)
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

let run_rb () =
  print_endline "RB tree scaling benchmarks:";
  let (_ : Benchmark.Result.t list) =
    Rb_bench.run_scaling_benchmark
      ~benchmark_config:(Benchmark.Config.create ~sample_runs:10 ())
      ~tree_sizes:[ 10; 100; 1000; 10_000; 100_000; 1_000_000; 10_000_000 ]
      ~num_operations:1000
      ()
    |> fun results ->
    Benchmark.print_results results;
    results
  in
  ()
;;

let run_map () =
  print_endline "Core Map scaling benchmarks:";
  let (_ : Benchmark.Result.t list) =
    Map_bench.run_scaling_benchmark
      ~benchmark_config:(Benchmark.Config.create ~sample_runs:10 ())
      ~tree_sizes:[ 10; 100; 1000; 10_000; 100_000; 1_000_000; 10_000_000 ]
      ~num_operations:1000
      ()
    |> fun results ->
    Benchmark.print_results results;
    results
  in
  ()
;;

let run_sat ~max_num_vars =
  printf "SAT solver benchmarks up to n=%d variables\n" max_num_vars;
  let (_ : Benchmark.Result.t list) =
    Sat_bench.run_scaling_benchmark ~max_num_vars ()
  in
  ()
;;

let run_dimacs () =
  print_endline "DIMACS example benchmarks:";
  let (_ : Benchmark.Result.t list) =
    Dimacs_bench.run_dimacs_examples ()
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
         ~doc:"BENCH Which benchmark to run (examples, rb, map, sat, dimacs, or all). Default: all"
     and sat_max_n =
       flag
         "sat-max-n"
         (optional int)
         ~doc:
           "INT Maximum number of variables to use for SAT benchmarks (default: 400)"
     in
     fun () ->
       let sat_max_n = Option.value sat_max_n ~default:400 in
       match bench with
       | None | Some "all" ->
         run_examples ();
         print_endline "\n";
         run_rb ();
         print_endline "\n";
         run_map ();
         print_endline "\n";
         run_sat ~max_num_vars:sat_max_n;
         print_endline "\n";
         run_dimacs ()
       | Some "examples" -> run_examples ()
       | Some "rb" -> run_rb ()
       | Some "map" -> run_map ()
       | Some "sat" -> run_sat ~max_num_vars:sat_max_n
       | Some "dimacs" -> run_dimacs ()
       | Some other ->
         eprintf "Unknown benchmark: %s\n" other;
         eprintf "Valid options: examples, rb, map, sat, dimacs, all\n";
         exit 1)
;;

let () = Command_unix.run command
