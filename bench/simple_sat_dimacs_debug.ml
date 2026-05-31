open! Core
open! Feel

let command =
  Command.basic
    ~summary:"Run one DIMACS example through Simple_sat and print timing"
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
       let size =
         Array.fold instance.clauses ~init:0 ~f:(fun acc clause ->
           Array.fold clause ~init:acc ~f:(fun acc literal ->
             Int.max acc (Int.abs literal)))
       in
       let start = Time_ns.now () in
       let result = Simple_sat.solve ~size instance.clauses in
       let elapsed = Time_ns.diff (Time_ns.now ()) start in
       print_s
         [%message
           ""
             (instance.name : string)
             (result : bool)
             (elapsed : Time_ns.Span.t)])
;;

let () = Command_unix.run command
