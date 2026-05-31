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
       let solver = Feel.Solver.create () in
       let start = Time_ns.now () in
       let add_result =
         Array.find_map instance.clauses ~f:(fun clause ->
           match Feel.Solver.add_clause solver ~clause with
           | `Ok -> None
           | `Unsat unsat_core -> Some unsat_core)
       in
       let result =
         match add_result with
         | Some unsat_core -> Feel.Sat_result.Unsat { unsat_core }
         | None -> Feel.Solver.solve solver
       in
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
             (result : string)
             (elapsed : Time_ns.Span.t)];
       print_s [%sexp (Feel.Solver.stats solver : Feel.Stats.t)])
;;

let () = Command_unix.run command
