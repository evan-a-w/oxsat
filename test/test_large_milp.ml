open! Core
open! Feel.Import
open! Theory

(* Bin packing: assign n items to bins of fixed capacity. Each item i has a
   binary variable x_[{i,b}] (item i goes in bin b) and each bin b has a binary
   variable y_b (bin b is used at all). Constraints:
   - each item in exactly one bin: sum_b x_[{i,b}] = 1
   - capacity: sum_i size_i * x_[{i,b}] <= cap * y_b
   - linking: x_[{i,b}] <= y_b

   This has a notoriously weak LP relaxation (integrality gap up to 3/2), so
   branch-and-bound must explore many nodes even on small instances. *)

let tvar name = Tvar.of_string name

let le expr c : Formula.any =
  La_compare (Encoding.linear_expr_to_formula expr, `Le, La_const (Q.of_int c))
;;

let ge expr c : Formula.any =
  La_compare (Encoding.linear_expr_to_formula expr, `Ge, La_const (Q.of_int c))
;;

let is_int v : Formula.any = Eq (Var v, Int)
let var v = Linear_expr.var v
let sum exprs = List.fold exprs ~init:Linear_expr.zero ~f:Linear_expr.( + )

let assert_ok solver formula =
  match Theory.Solver.assert_formula solver formula with
  | `Ok -> ()
  | `Unsat _ -> failwith "unexpectedly unsat at assert time"
;;

let make_bin_packing_solver ~sizes ~cap ~num_bins =
  let solver = Theory.Solver.create () in
  let n = List.length sizes in
  let x =
    Array.init n ~f:(fun i ->
      Array.init num_bins ~f:(fun b -> tvar (Printf.sprintf "x_%d_%d" i b)))
  in
  let y = Array.init num_bins ~f:(fun b -> tvar (Printf.sprintf "y_%d" b)) in
  Array.iter x ~f:(fun row ->
    Array.iter row ~f:(fun v ->
      assert_ok solver (is_int v);
      assert_ok solver (ge (var v) 0);
      assert_ok solver (le (var v) 1)));
  Array.iter y ~f:(fun v ->
    assert_ok solver (is_int v);
    assert_ok solver (ge (var v) 0);
    assert_ok solver (le (var v) 1));
  List.iteri sizes ~f:(fun i _ ->
    let row_sum = sum (Array.to_list (Array.map x.(i) ~f:var)) in
    assert_ok solver (le row_sum 1);
    assert_ok solver (ge row_sum 1));
  Array.iteri y ~f:(fun b y_b ->
    let load =
      sum
        (List.mapi sizes ~f:(fun i sz ->
           Linear_expr.scale (Q.of_int sz) (var x.(i).(b))))
    in
    let cap_expr = Linear_expr.(load - scale (Q.of_int cap) (var y_b)) in
    assert_ok solver (le cap_expr 0);
    Array.iter x ~f:(fun row ->
      let link = Linear_expr.(var row.(b) - var y_b) in
      assert_ok solver (le link 0)));
  solver
;;

let print_result result =
  match result with
  | Solver_result.Sat _ -> print_endline "SAT"
  | Unsat _ -> print_endline "UNSAT"
;;

(* Generate [n] item sizes in [1..max_size] using a simple LCG so instances are
   reproducible without a Core dependency on Random.State. *)
let gen_sizes ~n ~max_size ~seed =
  let state = ref seed in
  List.init n ~f:(fun _ ->
    state := ((!state * 1664525) + 1013904223) land 0x7fffffff;
    1 + (!state mod max_size))
;;

(* items [4;5;6;7], cap 10: 4+6=10 fills one bin, but 5+7=12>10 so 3 bins
   needed. *)
let%expect_test "bin packing 4 items cap 10: 2 bins infeasible" =
  let solver =
    make_bin_packing_solver ~sizes:[ 4; 5; 6; 7 ] ~cap:10 ~num_bins:2
  in
  print_result (Theory.Solver.solve solver);
  [%expect {| UNSAT |}]
;;

let%expect_test "bin packing 4 items cap 10: 3 bins feasible" =
  let solver =
    make_bin_packing_solver ~sizes:[ 4; 5; 6; 7 ] ~cap:10 ~num_bins:3
  in
  print_result (Theory.Solver.solve solver);
  [%expect {| SAT |}]
;;

(* 6 items [3;4;5;6;7;8], cap 11. (3+8)=11, (4+7)=11, (5+6)=11 — perfect fit in
   3 bins. *)
let%expect_test "bin packing 6 items cap 11: 2 bins infeasible" =
  let solver =
    make_bin_packing_solver ~sizes:[ 3; 4; 5; 6; 7; 8 ] ~cap:11 ~num_bins:2
  in
  print_result (Theory.Solver.solve solver);
  [%expect {| UNSAT |}]
;;

let%expect_test "bin packing 6 items cap 11: 3 bins feasible" =
  let solver =
    make_bin_packing_solver ~sizes:[ 3; 4; 5; 6; 7; 8 ] ~cap:11 ~num_bins:3
  in
  print_result (Theory.Solver.solve solver);
  [%expect {| SAT |}]
;;

(* --- Large instances --- Variable counts: n items * num_bins + num_bins.
   Timings measured on this machine; solve time grows steeply with variable
   count due to MILP branching on binary integrality constraints.

   n=20 k=4 => 84 vars ~0.06s n=30 k=5 => 155 vars ~0.4s n=40 k=6 => 246 vars
   ~1.7s

   Cap is set to ceil(total_weight / num_bins) so that a balanced packing is
   always feasible. *)

let ceil_div a b = (a + b - 1) / b

(* 20 items in 1..5, 4 bins. 84 binary variables. *)
let%expect_test "bin packing 20 items 4 bins: feasible" =
  let sizes = gen_sizes ~n:20 ~max_size:5 ~seed:42 in
  let cap = ceil_div (List.sum (module Int) sizes ~f:Fn.id) 4 in
  let solver = make_bin_packing_solver ~sizes ~cap ~num_bins:4 in
  print_result (Theory.Solver.solve solver);
  [%expect {| SAT |}]
;;

(* 30 items in 1..5, 5 bins. 155 binary variables. *)
let%expect_test "bin packing 30 items 5 bins: feasible" =
  let sizes = gen_sizes ~n:30 ~max_size:5 ~seed:137 in
  let cap = ceil_div (List.sum (module Int) sizes ~f:Fn.id) 5 in
  let solver = make_bin_packing_solver ~sizes ~cap ~num_bins:5 in
  print_result (Theory.Solver.solve solver);
  [%expect {| SAT |}]
;;

(* 40 items in 1..5, 6 bins. 246 binary variables. *)
let%expect_test "bin packing 40 items 6 bins: feasible" =
  let sizes = gen_sizes ~n:40 ~max_size:5 ~seed:999 in
  let cap = ceil_div (List.sum (module Int) sizes ~f:Fn.id) 6 in
  let solver = make_bin_packing_solver ~sizes ~cap ~num_bins:6 in
  print_result (Theory.Solver.solve solver);
  [%expect {| SAT |}]
;;
