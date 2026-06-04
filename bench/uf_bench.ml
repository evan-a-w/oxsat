open! Core
open! Feel
open! Theories

let solve_with_uf ~build =
  let uf = Uf.create () in
  let packed = Theory.Packed.create (module Uf) uf in
  let solver = Solver.create ~theory:packed () in
  build uf solver;
  ignore (Solver.solve solver : Sat_result.t)
;;

let transitivity_chain ~n =
  ( sprintf "UF transitivity chain (n=%d)" n
  , fun () ->
      solve_with_uf ~build:(fun uf solver ->
        let xs = Array.init n ~f:(fun _ -> Uf.new_const uf) in
        let fx0 = Uf.new_app uf ~func:0 ~args:[| xs.(0) |] in
        let fxn = Uf.new_app uf ~func:0 ~args:[| xs.(n - 1) |] in
        let _eq_fxfxn = Uf.register_eq uf ~lhs:fx0 ~rhs:fxn in
        let neq_fxfxn = Uf.register_neq uf ~lhs:fx0 ~rhs:fxn in
        let chain_eqs =
          Array.init (n - 1) ~f:(fun i ->
            Uf.register_eq uf ~lhs:xs.(i) ~rhs:xs.(i + 1))
        in
        ignore
          (Solver.add_clause solver ~clause:[| neq_fxfxn |]
           : [ `Ok | `Unsat of _ ]);
        Array.iter chain_eqs ~f:(fun eq ->
          ignore
            (Solver.add_clause solver ~clause:[| eq |] : [ `Ok | `Unsat of _ ])))
  )
;;

let wide_congruence ~width =
  ( sprintf "UF wide congruence conflict (width=%d)" width
  , fun () ->
      solve_with_uf ~build:(fun uf solver ->
        let xs = Array.init width ~f:(fun _ -> Uf.new_const uf) in
        let ys = Array.init width ~f:(fun _ -> Uf.new_const uf) in
        let fx = Uf.new_app uf ~func:0 ~args:xs in
        let fy = Uf.new_app uf ~func:0 ~args:ys in
        let _eq_fxfy = Uf.register_eq uf ~lhs:fx ~rhs:fy in
        let neq_fxfy = Uf.register_neq uf ~lhs:fx ~rhs:fy in
        let arg_eqs =
          Array.init width ~f:(fun i ->
            Uf.register_eq uf ~lhs:xs.(i) ~rhs:ys.(i))
        in
        ignore
          (Solver.add_clause solver ~clause:[| neq_fxfy |]
           : [ `Ok | `Unsat of _ ]);
        Array.iter arg_eqs ~f:(fun eq ->
          ignore
            (Solver.add_clause solver ~clause:[| eq |] : [ `Ok | `Unsat of _ ])))
  )
;;

let backtrack_stress ~n ~k =
  ( sprintf "UF backtrack stress (n=%d, k=%d)" n k
  , fun () ->
      solve_with_uf ~build:(fun uf solver ->
        let terms = Array.init n ~f:(fun _ -> Uf.new_const uf) in
        let colors = Array.init k ~f:(fun _ -> Uf.new_const uf) in
        let eq =
          Array.init n ~f:(fun i ->
            Array.init k ~f:(fun c ->
              Uf.register_eq uf ~lhs:terms.(i) ~rhs:colors.(c)))
        in
        Array.iter eq ~f:(fun row ->
          ignore (Solver.add_clause solver ~clause:row : [ `Ok | `Unsat of _ ]));
        for c1 = 0 to k - 2 do
          for c2 = c1 + 1 to k - 1 do
            let neq = Uf.register_neq uf ~lhs:colors.(c1) ~rhs:colors.(c2) in
            ignore
              (Solver.add_clause solver ~clause:[| neq |]
               : [ `Ok | `Unsat of _ ])
          done
        done;
        for i = 0 to n - 2 do
          let e = Uf.register_eq uf ~lhs:terms.(i) ~rhs:terms.(i + 1) in
          ignore
            (Solver.add_clause solver ~clause:[| e |] : [ `Ok | `Unsat of _ ])
        done) )
;;

let default_config =
  Benchmark.Config.create
    ~warmup_runs:1
    ~sample_runs:5
    ~target_time_ns:1_000_000_000
    ()
;;

let run_benchmark ?(config = default_config) () =
  Benchmark.run_all_and_print
    ~config
    [ transitivity_chain ~n:50
    ; transitivity_chain ~n:200
    ; transitivity_chain ~n:500
    ; wide_congruence ~width:10
    ; wide_congruence ~width:50
    ; backtrack_stress ~n:20 ~k:3
    ; backtrack_stress ~n:50 ~k:3
    ; backtrack_stress ~n:20 ~k:4
    ; backtrack_stress ~n:100 ~k:3
    ]
  |> ignore
;;
