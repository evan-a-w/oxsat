open! Core
open! Theory

module Instance = struct
  type t =
    { name : string
    ; formulas : Formula.any list
    }
end

let solve_instance (inst : Instance.t) =
  let solver = Solver.create () in
  List.iter inst.formulas ~f:(fun f ->
    ignore
      (Or_error.ok_exn (Solver.assert_formula solver f) : [ `Ok | `Unsat of _ ]));
  ignore (Solver.solve solver : Solver_result.t)
;;

let benchmark_of_instance inst =
  inst.Instance.name, fun () -> solve_instance inst
;;

(* --- helpers --- *)

let tvar name = Tvar.of_string name
let var t = Linear_expr.var t

let le expr c : Formula.any =
  La_compare (Encoding.linear_expr_to_formula expr, `Le, La_const (Q.of_int c))
;;

let ge expr c : Formula.any =
  La_compare (Encoding.linear_expr_to_formula expr, `Ge, La_const (Q.of_int c))
;;

let is_int t : Formula.any = Eq (Var t, Int)
let sum es = List.fold es ~init:Linear_expr.zero ~f:Linear_expr.( + )
let uft_var name : Formula.any = Var (tvar name)
let uft_app fn arg : Formula.any = App (tvar fn, [ arg ])
let eq_t a b : Formula.any = Eq (a, b)
let neq_t a b : Formula.any = Not (eq_t a b)

(* Simple LCG, same constants as in test_large_milp.ml. Always returns a
   positive 31-bit value. *)
let lcg state =
  state := ((!state * 1664525) + 1013904223) land 0x7fffffff;
  !state
;;

let lcg_int state lo hi = lo + (lcg state mod (hi - lo + 1))

(* --- LP (real variables) --- *)

(** Satisfiable LP: plant x_i = i+1 and generate random constraints that the
    planted solution satisfies with slack 5. Uses only non-negative coefficients
    so x=0 is always feasible too, keeping the feasible region large and the
    simplex fast. Exercises LP theory with no SAT case splits. *)
let gen_lp_feasible ~num_vars ~num_constraints ~seed =
  let state = ref seed in
  let vs = Array.init num_vars ~f:(fun i -> tvar (sprintf "lp_%d" i)) in
  let planted = Array.init num_vars ~f:(fun i -> i + 1) in
  let bounds =
    Array.to_list vs
    |> List.concat_map ~f:(fun v -> [ ge (var v) 0; le (var v) (num_vars + 5) ])
  in
  let constraints =
    List.init num_constraints ~f:(fun _ ->
      let k = 3 + lcg_int state 0 2 in
      let terms =
        List.init k ~f:(fun _ ->
          let vi = lcg_int state 0 (num_vars - 1) in
          let ci = 1 + lcg_int state 0 2 in
          vi, ci)
      in
      let lhs_val =
        List.fold terms ~init:0 ~f:(fun acc (vi, ci) ->
          acc + (ci * planted.(vi)))
      in
      let expr =
        List.fold terms ~init:Linear_expr.zero ~f:(fun acc (vi, ci) ->
          Linear_expr.(acc + scale (Q.of_int ci) (var vs.(vi))))
      in
      le expr (lhs_val + 5))
  in
  Instance.
    { name = sprintf "LP feasible (n=%d, m=%d)" num_vars num_constraints
    ; formulas = bounds @ constraints
    }
;;

(** LP with forced discrete choice: each variable must be in [0,30] ("low") or
    [70,100] ("high"), with a sum lower bound that requires ~43% of vars to be
    high. The SAT solver must find which vars to make high via LP-guided
    backtracking.

    Analysis: with k high vars and n-k low vars, max sum = 100k + 30(n-k). For
    sum >= 60n: 70k >= 30n, so k >= ceil(3n/7) ≈ 0.43n. With k-1 high, LP
    detects infeasibility and forces backtracking. *)
let gen_lp_tight_or ~num_vars ~seed =
  ignore seed;
  let vs = Array.init num_vars ~f:(fun i -> tvar (sprintf "tlp_%d" i)) in
  let bounds =
    Array.to_list vs
    |> List.concat_map ~f:(fun v -> [ ge (var v) 0; le (var v) 100 ])
  in
  let choices =
    Array.to_list vs
    |> List.map ~f:(fun v -> Formula.Or [ le (var v) 30; ge (var v) 70 ])
  in
  let sum_lb = ge (sum (Array.to_list (Array.map vs ~f:var))) (60 * num_vars) in
  Instance.
    { name = sprintf "LP tight OR (n=%d)" num_vars
    ; formulas = bounds @ choices @ [ sum_lb ]
    }
;;

(* --- MILP (integer variables via type theory) --- *)

(** Bin packing: assign items of random sizes to bins of capacity
    ceil(total/num_bins), always feasible. Same structure as test_large_milp.ml.
    Exercises branch-and-bound. *)
let gen_bin_packing ~num_items ~num_bins ~max_size ~seed =
  let state = ref seed in
  let sizes = List.init num_items ~f:(fun _ -> 1 + (lcg state mod max_size)) in
  let total = List.sum (module Int) sizes ~f:Fn.id in
  let cap = (total + num_bins - 1) / num_bins in
  let x =
    Array.init num_items ~f:(fun i ->
      Array.init num_bins ~f:(fun b -> tvar (sprintf "bp_x%d_%d" i b)))
  in
  let y = Array.init num_bins ~f:(fun b -> tvar (sprintf "bp_y%d" b)) in
  let fs = ref [] in
  let add f = fs := f :: !fs in
  Array.iter x ~f:(fun row ->
    Array.iter row ~f:(fun v ->
      add (is_int v);
      add (ge (var v) 0);
      add (le (var v) 1)));
  Array.iter y ~f:(fun v ->
    add (is_int v);
    add (ge (var v) 0);
    add (le (var v) 1));
  List.iteri sizes ~f:(fun i _ ->
    let s = sum (Array.to_list (Array.map x.(i) ~f:var)) in
    add (le s 1);
    add (ge s 1));
  Array.iteri y ~f:(fun b yb ->
    let load =
      sum
        (List.mapi sizes ~f:(fun i sz ->
           Linear_expr.scale (Q.of_int sz) (var x.(i).(b))))
    in
    add (le Linear_expr.(load - scale (Q.of_int cap) (var yb)) 0);
    Array.iter x ~f:(fun row -> add (le Linear_expr.(var row.(b) - var yb) 0)));
  Instance.
    { name = sprintf "Bin packing (items=%d, bins=%d)" num_items num_bins
    ; formulas = !fs
    }
;;

(* --- EUF instances --- *)

(** Equality chain: x_0=x_1=...=x_[{n-1}], then f(x_0) != f(x_[{n-1}]). UNSAT by
    congruence closure. Tests how quickly the union-find + conflict detection
    handles long chains. *)
let gen_euf_chain ~num_vars =
  let vs = Array.init num_vars ~f:(fun i -> uft_var (sprintf "cv%d" i)) in
  let equalities =
    List.init (num_vars - 1) ~f:(fun i -> eq_t vs.(i) vs.(i + 1))
  in
  let conflict = neq_t (uft_app "cf" vs.(0)) (uft_app "cf" vs.(num_vars - 1)) in
  Instance.
    { name = sprintf "EUF chain UNSAT (n=%d)" num_vars
    ; formulas = equalities @ [ conflict ]
    }
;;

(** Two-group EUF: vars split into two independent equivalence classes, with
    cross-group disequalities. SAT. Exercises the congruence closure on a
    satisfiable instance. *)
let gen_euf_two_groups ~num_vars =
  let half = num_vars / 2 in
  let vs = Array.init num_vars ~f:(fun i -> uft_var (sprintf "gv%d" i)) in
  let eq0 = List.init (half - 1) ~f:(fun i -> eq_t vs.(i) vs.(i + 1)) in
  let eq1 =
    List.init
      (num_vars - half - 1)
      ~f:(fun i -> eq_t vs.(half + i) vs.(half + i + 1))
  in
  let diseqs =
    [ neq_t (uft_app "gf" vs.(0)) (uft_app "gf" vs.(half))
    ; neq_t (uft_app "gf" vs.(1)) (uft_app "gf" vs.(half + 1))
    ]
  in
  Instance.
    { name = sprintf "EUF two-group SAT (n=%d)" num_vars
    ; formulas = eq0 @ eq1 @ diseqs
    }
;;

(** Random congruence web: many equalities between random variable pairs over a
    set of shared variables, plus function applications with disequalities. The
    congruence closure must propagate equality through a dense graph. Outcome
    (SAT/UNSAT) depends on the random seed. *)
let gen_euf_congruence_web ~num_vars ~num_functions ~seed =
  let state = ref seed in
  let vs = Array.init num_vars ~f:(fun i -> uft_var (sprintf "wv%d" i)) in
  let fnames = Array.init num_functions ~f:(fun i -> sprintf "wf%d" i) in
  let equalities =
    List.init (num_vars * 2) ~f:(fun _ ->
      let i = lcg_int state 0 (num_vars - 1) in
      let j = lcg_int state 0 (num_vars - 1) in
      if i = j then Formula.True else eq_t vs.(i) vs.(j))
  in
  let apps =
    Array.init num_functions ~f:(fun fi ->
      Array.init num_vars ~f:(fun vi -> uft_app fnames.(fi) vs.(vi)))
  in
  let diseqs =
    List.init (num_vars / 2) ~f:(fun _ ->
      let fi = lcg_int state 0 (num_functions - 1) in
      let i = lcg_int state 0 (num_vars - 1) in
      let j = lcg_int state 0 (num_vars - 1) in
      if i = j then Formula.True else neq_t apps.(fi).(i) apps.(fi).(j))
  in
  Instance.
    { name = sprintf "EUF web (n=%d, f=%d)" num_vars num_functions
    ; formulas = equalities @ diseqs
    }
;;

(* --- benchmark runner --- *)

let default_benchmark_config =
  Benchmark.Config.create
    ~warmup_runs:1
    ~sample_runs:3
    ~target_time_ns:2_000_000_000
    ()
;;

let run_scaling_benchmark ?(benchmark_config = default_benchmark_config) () =
  let instances =
    [ gen_lp_feasible ~num_vars:10 ~num_constraints:30 ~seed:42
    ; gen_lp_feasible ~num_vars:20 ~num_constraints:60 ~seed:42
    ; gen_lp_feasible ~num_vars:50 ~num_constraints:150 ~seed:42
    ; gen_lp_tight_or ~num_vars:5 ~seed:42
    ; gen_lp_tight_or ~num_vars:7 ~seed:42
    ; gen_lp_tight_or ~num_vars:9 ~seed:42
    ; gen_bin_packing ~num_items:10 ~num_bins:3 ~max_size:5 ~seed:42
    ; gen_bin_packing ~num_items:15 ~num_bins:4 ~max_size:5 ~seed:137
    ; gen_bin_packing ~num_items:20 ~num_bins:4 ~max_size:5 ~seed:999
    ; gen_euf_chain ~num_vars:50
    ; gen_euf_chain ~num_vars:200
    ; gen_euf_chain ~num_vars:1000
    ; gen_euf_two_groups ~num_vars:50
    ; gen_euf_two_groups ~num_vars:200
    ; gen_euf_congruence_web ~num_vars:20 ~num_functions:3 ~seed:42
    ; gen_euf_congruence_web ~num_vars:50 ~num_functions:5 ~seed:137
    ]
  in
  (* Run each benchmark individually and print immediately so slow instances are
     visible. *)
  List.concat_map instances ~f:(fun inst ->
    let results =
      Benchmark.run_all_and_print
        ~config:benchmark_config
        [ benchmark_of_instance inst ]
    in
    results)
;;
