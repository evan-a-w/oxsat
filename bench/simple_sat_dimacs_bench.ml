open! Core
open! Feel

module Prepared_instance = struct
  type t =
    { name : string
    ; size : int
    ; clauses : int array array
    }
end

let dimacs_size dimacs clauses =
  let parse_header line =
    let pieces =
      String.split ~on:' ' (String.strip line)
      |> List.filter ~f:(Fn.non String.is_empty)
    in
    match pieces with
    | [ "p"; "cnf"; size; _num_clauses ] -> Int.of_string_opt size
    | _ -> None
  in
  match List.find_map (String.split_lines dimacs) ~f:parse_header with
  | Some size -> size
  | None ->
    Array.fold clauses ~init:0 ~f:(fun acc clause ->
      Array.fold clause ~init:acc ~f:(fun acc literal ->
        Int.max acc (Int.abs literal)))
;;

let prepare_instance (instance : Dimacs_bench.Instance.t) =
  Prepared_instance.
    { name = instance.name
    ; size = dimacs_size instance.dimacs instance.clauses
    ; clauses = instance.clauses
    }
;;

let benchmark_of_instance (instance : Prepared_instance.t) =
  let run () =
    ignore (Simple_sat.solve ~size:instance.size instance.clauses : bool)
  in
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
  Dimacs_bench.default_instances ()
  |> List.map ~f:prepare_instance
  |> List.map ~f:benchmark_of_instance
  |> Benchmark.run_all_and_print ~config:benchmark_config
;;
