open! Core

let command =
  Command.basic
    ~summary:"Run one DIMACS example and print solver stats"
    (let open Command.Let_syntax in
     let%map_open instance_name =
       flag
         "instance"
         (optional_with_default "SUBSETS_100" string)
         ~doc:"NAME DIMACS example name, e.g. SUBSETS_100"
     and solver_name =
       flag
         "solver"
         (optional_with_default "feel" string)
         ~doc:"NAME Solver to run: feel or simple"
     and profile =
       flag "profile" no_arg ~doc:"Include optional timing profile in stats"
     in
     fun () ->
       let instances = Dimacs_bench.default_instances () in
       let instance =
         List.find instances ~f:(fun instance ->
           String.Caseless.equal instance.name instance_name)
         |> Option.value_or_thunk ~default:(fun () ->
           let valid =
             instances
             |> List.map ~f:(fun instance -> instance.name)
             |> String.concat ~sep:", "
           in
           failwithf
             "unknown DIMACS instance %S. Valid: %s"
             instance_name
             valid
             ())
       in
       match String.lowercase solver_name with
       | "feel" ->
         let solver = Feel.Solver.create ~profile () in
         Array.iter instance.clauses ~f:(fun clause ->
           ignore (Feel.Solver.add_clause' solver ~clause));
         let start = Time_ns.now () in
         let result = Feel.Solver.solve solver in
         let elapsed = Time_ns.diff (Time_ns.now ()) start in
         let result =
           match result with
           | Sat _ -> "sat"
           | Unsat _ -> "unsat"
         in
         print_s
           [%message
             ""
               (instance.name : string)
               (solver_name : string)
               (result : string)
               (elapsed : Time_ns.Span.t)];
         print_s [%sexp (Feel.Solver.stats solver : Feel.Solver.Stats.t)]
       | "simple" | "simple_sat" ->
         let start = Time_ns.now () in
         let result, stats =
           Feel.Simple_sat.solve_with_stats
             ~profile
             ~size:
               (Array.fold instance.clauses ~init:0 ~f:(fun acc clause ->
                  Array.fold clause ~init:acc ~f:(fun acc literal ->
                    Int.max acc (Int.abs literal))))
             instance.clauses
         in
         let elapsed = Time_ns.diff (Time_ns.now ()) start in
         let result = if result then "sat" else "unsat" in
         print_s
           [%message
             ""
               (instance.name : string)
               (solver_name : string)
               (result : string)
               (elapsed : Time_ns.Span.t)];
         print_s [%sexp (stats : Feel.Simple_sat.Stats.t)]
       | other -> failwithf "unknown solver %S. Valid: feel, simple" other ())
;;

let () = Command_unix.run command
