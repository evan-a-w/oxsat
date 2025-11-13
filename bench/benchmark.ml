open! Core

module Config = struct
  type t =
    { warmup_runs : int
    ; sample_runs : int
    ; min_iterations : int
    ; max_iterations : int
    ; target_time_ns : int
    }

  let default =
    { warmup_runs = 1
    ; sample_runs = 100
    ; min_iterations = 1
    ; max_iterations = 1_000_000_000
    ; target_time_ns = 100_000_000 (* 100ms *)
    }
  ;;

  let create
    ?warmup_runs
    ?sample_runs
    ?min_iterations
    ?max_iterations
    ?target_time_ns
    ()
    =
    { warmup_runs = Option.value warmup_runs ~default:default.warmup_runs
    ; sample_runs = Option.value sample_runs ~default:default.sample_runs
    ; min_iterations = Option.value min_iterations ~default:default.min_iterations
    ; max_iterations = Option.value max_iterations ~default:default.max_iterations
    ; target_time_ns = Option.value target_time_ns ~default:default.target_time_ns
    }
  ;;
end

module Stats = struct
  type t =
    { mean : float
    ; stddev : float
    ; p50 : float
    ; p75 : float
    ; p90 : float
    ; p95 : float
    ; p99 : float
    ; samples : int
    }
  [@@deriving sexp_of]

  let calculate (samples : float array) =
    let n = Array.length samples in
    if n = 0
    then
      failwith "Cannot calculate stats on empty sample set";
    Array.sort samples ~compare:Float.compare;
    let mean = Array.fold samples ~init:0. ~f:( +. ) /. Float.of_int n in
    let variance =
      Array.fold samples ~init:0. ~f:(fun acc x ->
        let diff = x -. mean in
        acc +. (diff *. diff))
      /. Float.of_int n
    in
    let stddev = Float.sqrt variance in
    let percentile p =
      let idx = Float.to_int (Float.of_int n *. p /. 100.) in
      let idx = Int.min idx (n - 1) in
      samples.(idx)
    in
    { mean
    ; stddev
    ; p50 = percentile 50.
    ; p75 = percentile 75.
    ; p90 = percentile 90.
    ; p95 = percentile 95.
    ; p99 = percentile 99.
    ; samples = n
    }
  ;;

  let to_string_ns t =
    sprintf
      "Mean: %.2f ns  StdDev: %.2f ns  p50: %.2f ns  p75: %.2f ns  p90: %.2f ns  \
       p95: %.2f ns  p99: %.2f ns  (n=%d)"
      t.mean
      t.stddev
      t.p50
      t.p75
      t.p90
      t.p95
      t.p99
      t.samples
  ;;

  let to_string_readable t =
    let format_time ns =
      if Float.(ns < 1000.)
      then sprintf "%.2f ns" ns
      else if Float.(ns < 1_000_000.)
      then sprintf "%.2f μs" (ns /. 1000.)
      else if Float.(ns < 1_000_000_000.)
      then sprintf "%.2f ms" (ns /. 1_000_000.)
      else sprintf "%.2f s" (ns /. 1_000_000_000.)
    in
    sprintf
      "Mean: %s  StdDev: %s  p50: %s  p75: %s  p90: %s  p95: %s  p99: %s  (n=%d)"
      (format_time t.mean)
      (format_time t.stddev)
      (format_time t.p50)
      (format_time t.p75)
      (format_time t.p90)
      (format_time t.p95)
      (format_time t.p99)
      t.samples
  ;;
end

module Result = struct
  type t =
    { name : string
    ; stats : Stats.t
    }
  [@@deriving sexp_of]

  let to_string t = sprintf "%s: %s" t.name (Stats.to_string_readable t.stats)
end

let time_once f =
  let start = Time_ns.now () in
  let (_ : _) = f () in
  let end_ = Time_ns.now () in
  Time_ns.diff end_ start |> Time_ns.Span.to_ns
;;

let estimate_iterations config f =
  let elapsed_ns = time_once f in
  if Float.(elapsed_ns <= 0.)
  then config.Config.max_iterations
  else (
    let estimated = Float.to_int (Float.of_int config.Config.target_time_ns /. elapsed_ns) in
    Int.clamp_exn estimated ~min:config.Config.min_iterations ~max:config.Config.max_iterations)
;;

let time_iterations n f =
  let start = Time_ns.now () in
  for _ = 1 to n do
    let (_ : _) = f () in
    ()
  done;
  let end_ = Time_ns.now () in
  Time_ns.diff end_ start |> Time_ns.Span.to_ns
;;

let benchmark_function ?(config = Config.default) f =
  (* Warmup *)
  for _ = 1 to config.warmup_runs do
    let (_ : float) = time_once f in
    ()
  done;
  (* Estimate iterations *)
  let iterations = estimate_iterations config f in
  (* Collect samples *)
  let samples = Array.init config.sample_runs ~f:(fun _ ->
    time_iterations iterations f /. Float.of_int iterations)
  in
  Stats.calculate samples
;;

let run ?(config = Config.default) ~name f =
  let stats = benchmark_function ~config f in
  { Result.name; stats }
;;

let run_all ?(config = Config.default) benchmarks =
  List.map benchmarks ~f:(fun (name, f) ->
    let stats = benchmark_function ~config f in
    { Result.name; stats })
;;

let print_results results =
  List.iter results ~f:(fun result -> print_endline (Result.to_string result))
;;

let run_and_print ?(config = Config.default) ~name f =
  let result = run ~config ~name f in
  print_endline (Result.to_string result);
  result
;;

let run_all_and_print ?(config = Config.default) benchmarks =
  let results = run_all ~config benchmarks in
  print_results results;
  results
;;
