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
    ; min_iterations =
        Option.value min_iterations ~default:default.min_iterations
    ; max_iterations =
        Option.value max_iterations ~default:default.max_iterations
    ; target_time_ns =
        Option.value target_time_ns ~default:default.target_time_ns
    }
  ;;
end

module Sample = struct
  (* Per-iteration measurements for one sample run. *)
  type t =
    { time_ns : float
    ; alloc_bytes : float
    ; major_collections : float
    }
  [@@deriving sexp]
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
    ; alloc_bytes : float
    ; major_collections : float
    }
  [@@deriving sexp]

  let calculate (samples : Sample.t array) =
    let n = Array.length samples in
    if n = 0 then failwith "Cannot calculate stats on empty sample set";
    let times = Array.map samples ~f:(fun s -> s.Sample.time_ns) in
    Array.sort times ~compare:Float.compare;
    let mean = Array.fold times ~init:0. ~f:( +. ) /. Float.of_int n in
    let variance =
      Array.fold times ~init:0. ~f:(fun acc x ->
        let diff = x -. mean in
        acc +. (diff *. diff))
      /. Float.of_int n
    in
    let stddev = Float.sqrt variance in
    let percentile p =
      let idx = Float.to_int (Float.of_int n *. p /. 100.) in
      let idx = Int.min idx (n - 1) in
      times.(idx)
    in
    let average_of ~f =
      Array.fold samples ~init:0. ~f:(fun acc s -> acc +. f s) /. Float.of_int n
    in
    { mean
    ; stddev
    ; p50 = percentile 50.
    ; p75 = percentile 75.
    ; p90 = percentile 90.
    ; p95 = percentile 95.
    ; p99 = percentile 99.
    ; samples = n
    ; alloc_bytes = average_of ~f:(fun s -> s.Sample.alloc_bytes)
    ; major_collections = average_of ~f:(fun s -> s.Sample.major_collections)
    }
  ;;

  let format_time ns =
    if Float.(ns < 1000.)
    then sprintf "%.2f ns" ns
    else if Float.(ns < 1_000_000.)
    then sprintf "%.2f μs" (ns /. 1000.)
    else if Float.(ns < 1_000_000_000.)
    then sprintf "%.2f ms" (ns /. 1_000_000.)
    else sprintf "%.2f s" (ns /. 1_000_000_000.)
  ;;

  let format_bytes bytes =
    if Float.(bytes < 1024.)
    then sprintf "%.0f B" bytes
    else if Float.(bytes < 1024. ** 2.)
    then sprintf "%.2f KiB" (bytes /. 1024.)
    else if Float.(bytes < 1024. ** 3.)
    then sprintf "%.2f MiB" (bytes /. (1024. ** 2.))
    else sprintf "%.2f GiB" (bytes /. (1024. ** 3.))
  ;;

  let to_string_ns t =
    sprintf
      "Mean: %.2f ns  StdDev: %.2f ns  p50: %.2f ns  p75: %.2f ns  p90: %.2f \
       ns  p95: %.2f ns  p99: %.2f ns  (n=%d)"
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
    sprintf
      "Mean: %s  StdDev: %s  p50: %s  p75: %s  p90: %s  p95: %s  p99: %s  \
       (n=%d)  Alloc: %s/iter  MajGC: %.3f/iter"
      (format_time t.mean)
      (format_time t.stddev)
      (format_time t.p50)
      (format_time t.p75)
      (format_time t.p90)
      (format_time t.p95)
      (format_time t.p99)
      t.samples
      (format_bytes t.alloc_bytes)
      t.major_collections
  ;;
end

module Result = struct
  type t =
    { name : string
    ; stats : Stats.t
    }
  [@@deriving sexp]

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
    let estimated =
      Float.to_int (Float.of_int config.Config.target_time_ns /. elapsed_ns)
    in
    Int.clamp_exn
      estimated
      ~min:config.Config.min_iterations
      ~max:config.Config.max_iterations)
;;

let time_iterations n f =
  let alloc_start = Gc.allocated_bytes () in
  let gc_start = Gc.quick_stat () in
  let start = Time_ns.now () in
  for _ = 1 to n do
    let (_ : _) = f () in
    ()
  done;
  let end_ = Time_ns.now () in
  let alloc_end = Gc.allocated_bytes () in
  let gc_end = Gc.quick_stat () in
  let n_float = Float.of_int n in
  { Sample.time_ns = (Time_ns.diff end_ start |> Time_ns.Span.to_ns) /. n_float
  ; alloc_bytes = (alloc_end -. alloc_start) /. n_float
  ; major_collections =
      Float.of_int (gc_end.major_collections - gc_start.major_collections)
      /. n_float
  }
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
  let samples =
    Array.init config.sample_runs ~f:(fun _ -> time_iterations iterations f)
  in
  Stats.calculate samples
;;

let run ?(config = Config.default) ~name f =
  let stats = benchmark_function ~config f in
  { Result.name; stats }
;;

let matches_only ~only name =
  match only with
  | [] -> true
  | substrings ->
    List.exists substrings ~f:(fun substring ->
      String.is_substring name ~substring)
;;

let filter_by_name ~only benchmarks =
  match only with
  | [] -> benchmarks
  | _ -> List.filter benchmarks ~f:(fun (name, _) -> matches_only ~only name)
;;

let run_all ?(config = Config.default) ?(only = []) benchmarks =
  let benchmarks = filter_by_name ~only benchmarks in
  if List.is_empty benchmarks && not (List.is_empty only)
  then eprintf "Warning: -only filters matched no benchmarks\n";
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

let run_all_and_print ?(config = Config.default) ?(only = []) benchmarks =
  let results = run_all ~config ~only benchmarks in
  print_results results;
  results
;;

let save_results ~filename results =
  Sexp.save_hum filename ([%sexp_of: Result.t list] results)
;;

let load_results ~filename =
  Sexp.load_sexp_conv_exn filename [%of_sexp: Result.t list]
;;

module Compare_row = struct
  type t =
    { name : string
    ; before : Stats.t
    ; after : Stats.t
    }

  let time_ratio t = t.after.mean /. t.before.mean

  let alloc_ratio t =
    if Float.(t.before.alloc_bytes > 0.)
    then Some (t.after.alloc_bytes /. t.before.alloc_bytes)
    else None
  ;;
end

let geomean ratios =
  Float.exp
    (List.sum (module Float) ratios ~f:Float.log
     /. Float.of_int (List.length ratios))
;;

let compare_results ~before ~after =
  let before_by_name =
    String.Map.of_alist_multi
      (List.map before ~f:(fun (r : Result.t) -> r.name, r))
    |> Map.map ~f:List.hd_exn
  in
  let rows =
    List.filter_map after ~f:(fun (r : Result.t) ->
      match Map.find before_by_name r.name with
      | Some before ->
        Some
          { Compare_row.name = r.name; before = before.stats; after = r.stats }
      | None ->
        eprintf "Warning: %s not present in before file, skipping\n" r.name;
        None)
  in
  let after_names = String.Set.of_list (List.map after ~f:(fun r -> r.name)) in
  List.iter before ~f:(fun (r : Result.t) ->
    if not (Set.mem after_names r.name)
    then eprintf "Warning: %s not present in after file, skipping\n" r.name);
  let rows =
    List.sort rows ~compare:(fun a b ->
      Float.compare (Compare_row.time_ratio b) (Compare_row.time_ratio a))
  in
  if List.is_empty rows
  then print_endline "No benchmarks in common."
  else (
    let name_width =
      List.fold rows ~init:4 ~f:(fun acc row ->
        Int.max acc (String.length row.Compare_row.name))
    in
    let print_row name before after time alloc =
      let name =
        name ^ String.make (Int.max 0 (name_width - String.length name)) ' '
      in
      printf "%s  %10s  %10s  %6s  %6s\n" name before after time alloc
    in
    print_row "name" "before" "after" "time" "alloc";
    List.iter rows ~f:(fun row ->
      let alloc =
        match Compare_row.alloc_ratio row with
        | Some ratio -> sprintf "%.2fx" ratio
        | None -> "-"
      in
      print_row
        row.name
        (Stats.format_time row.before.mean)
        (Stats.format_time row.after.mean)
        (sprintf "%.2fx" (Compare_row.time_ratio row))
        alloc);
    let time_geomean = geomean (List.map rows ~f:Compare_row.time_ratio) in
    let alloc_geomean =
      match List.filter_map rows ~f:Compare_row.alloc_ratio with
      | [] -> "-"
      | ratios -> sprintf "%.2fx" (geomean ratios)
    in
    print_row "geomean" "" "" (sprintf "%.2fx" time_geomean) alloc_geomean)
;;
