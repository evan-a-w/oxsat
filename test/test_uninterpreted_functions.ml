open! Core
open! Feel
open! Theories

let solve_with_uf ~build =
  let uf = Uf.create () in
  let packed = Theory.Packed.create (module Uf) uf in
  let solver = Solver.create ~theory:packed () in
  build uf solver;
  let result = Solver.solve solver in
  print_s [%sexp (result : Sat_result.t)]
;;

let%expect_test "transitivity conflict is unsat" =
  solve_with_uf ~build:(fun uf solver ->
    let x = Uf.new_const uf in
    let y = Uf.new_const uf in
    let z = Uf.new_const uf in
    let eq_xy = Uf.register_eq uf ~lhs:x ~rhs:y in
    let eq_yz = Uf.register_eq uf ~lhs:y ~rhs:z in
    let neq_xz = Uf.register_neq uf ~lhs:x ~rhs:z in
    ignore (Solver.add_clause solver ~clause:[| eq_xy |] : [ `Ok | `Unsat of _ ]);
    ignore (Solver.add_clause solver ~clause:[| eq_yz |] : [ `Ok | `Unsat of _ ]);
    ignore (Solver.add_clause solver ~clause:[| neq_xz |] : [ `Ok | `Unsat of _ ]));
  [%expect {| (Unsat (unsat_core (1))) |}]
;;

let%expect_test "consistent equality and disequality is sat" =
  solve_with_uf ~build:(fun uf solver ->
    let x = Uf.new_const uf in
    let y = Uf.new_const uf in
    let z = Uf.new_const uf in
    let eq_xy = Uf.register_eq uf ~lhs:x ~rhs:y in
    let neq_yz = Uf.register_neq uf ~lhs:y ~rhs:z in
    ignore (Solver.add_clause solver ~clause:[| eq_xy |] : [ `Ok | `Unsat of _ ]);
    ignore (Solver.add_clause solver ~clause:[| neq_yz |] : [ `Ok | `Unsat of _ ]));
  [%expect {| (Sat (assignments (() (true) (true)))) |}]
;;

let%expect_test "congruence conflict is unsat" =
  solve_with_uf ~build:(fun uf solver ->
    let x = Uf.new_const uf in
    let y = Uf.new_const uf in
    let fx = Uf.new_app uf ~func:0 ~args:[| x |] in
    let fy = Uf.new_app uf ~func:0 ~args:[| y |] in
    let eq_xy = Uf.register_eq uf ~lhs:x ~rhs:y in
    let _eq_fxfy = Uf.register_eq uf ~lhs:fx ~rhs:fy in
    let neq_fxfy = Uf.register_neq uf ~lhs:fx ~rhs:fy in
    ignore (Solver.add_clause solver ~clause:[| eq_xy |] : [ `Ok | `Unsat of _ ]);
    ignore (Solver.add_clause solver ~clause:[| neq_fxfy |] : [ `Ok | `Unsat of _ ]));
  [%expect {| (Unsat (unsat_core (1))) |}]
;;

let%expect_test "theory pop: backtrack restores consistency" =
  solve_with_uf ~build:(fun uf solver ->
    let x = Uf.new_const uf in
    let y = Uf.new_const uf in
    let z = Uf.new_const uf in
    let eq_xy = Uf.register_eq uf ~lhs:x ~rhs:y in
    let eq_yz = Uf.register_eq uf ~lhs:y ~rhs:z in
    let neq_xz = Uf.register_neq uf ~lhs:x ~rhs:z in
    ignore (Solver.add_clause solver ~clause:[| eq_xy; eq_yz |] : [ `Ok | `Unsat of _ ]);
    ignore (Solver.add_clause solver ~clause:[| neq_xz |] : [ `Ok | `Unsat of _ ]));
  [%expect {| (Sat (assignments (() (false) (true) (true)))) |}]
;;

(* Many terms forced into one equivalence class, then a disequality added
   at the end. The solver must backtrack repeatedly through many theory
   merges before concluding UNSAT, exercising pop on deep undo trails. *)
let%expect_test "deep undo trail: long chain then disequality is unsat" =
  solve_with_uf ~build:(fun uf solver ->
    let n = 30 in
    let xs = Array.init n ~f:(fun _ -> Uf.new_const uf) in
    let chain_eqs =
      Array.init (n - 1) ~f:(fun i -> Uf.register_eq uf ~lhs:xs.(i) ~rhs:xs.(i + 1))
    in
    let neq = Uf.register_neq uf ~lhs:xs.(0) ~rhs:xs.(n - 1) in
    Array.iter chain_eqs ~f:(fun eq ->
      ignore (Solver.add_clause solver ~clause:[| eq |] : [ `Ok | `Unsat of _ ]));
    ignore (Solver.add_clause solver ~clause:[| neq |] : [ `Ok | `Unsat of _ ]));
  [%expect {| (Unsat (unsat_core (1))) |}]
;;

let%expect_test "congruence propagation triggers conflict" =
  solve_with_uf ~build:(fun uf solver ->
    let x = Uf.new_const uf in
    let y = Uf.new_const uf in
    let fx = Uf.new_app uf ~func:0 ~args:[| x |] in
    let fy = Uf.new_app uf ~func:0 ~args:[| y |] in
    let eq_xy = Uf.register_eq uf ~lhs:x ~rhs:y in
    let eq_fxfy = Uf.register_eq uf ~lhs:fx ~rhs:fy in
    ignore (Solver.add_clause solver ~clause:[| eq_xy |] : [ `Ok | `Unsat of _ ]);
    ignore (Solver.add_clause solver ~clause:[| -eq_fxfy |] : [ `Ok | `Unsat of _ ]));
  [%expect {| (Unsat (unsat_core (1))) |}]
;;
