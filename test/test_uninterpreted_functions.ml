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

(* ───────────────────────────────────────────────────────────────────────── *)

(* Transitivity conflict: the Boolean formula alone is satisfiable (any
   assignment to the three variables works), but the theory makes it UNSAT.
   Specifically: (eq_xy) ∧ (eq_yz) ∧ (neq_xz) is boolean-SAT but
   theory-UNSAT because x=y ∧ y=z → x=z, contradicting x≠z.
   We force all three true with unit clauses — the boolean solver finds
   this satisfiable, but theory detects the conflict at quiescence. *)
let%expect_test "transitivity conflict is unsat" =
  solve_with_uf ~build:(fun uf solver ->
    let x = Uf.new_const uf in
    let y = Uf.new_const uf in
    let z = Uf.new_const uf in
    (* vars 1,2,3 mean: x=y, y=z, x≠z *)
    ignore (Uf.register_eq uf ~var:1 ~lhs:x ~rhs:y : int);
    ignore (Uf.register_eq uf ~var:2 ~lhs:y ~rhs:z : int);
    ignore (Uf.register_neq uf ~var:3 ~lhs:x ~rhs:z : int);
    (* Boolean formula: all three must be true — satisfiable in isolation *)
    ignore (Solver.add_clause solver ~clause:[| 1 |] : [ `Ok | `Unsat of _ ]);
    ignore (Solver.add_clause solver ~clause:[| 2 |] : [ `Ok | `Unsat of _ ]);
    ignore (Solver.add_clause solver ~clause:[| 3 |] : [ `Ok | `Unsat of _ ]));
  [%expect {| (Unsat (unsat_core (1))) |}]
;;

(* ───────────────────────────────────────────────────────────────────────── *)

(* Satisfiable: x=y and y≠z. No transitivity issue. Boolean formula requires
   var 1 (x=y) true and var 2 (y≠z) true. Theory: x and y are merged, z is
   separate — no conflict. *)
let%expect_test "consistent equality and disequality is sat" =
  solve_with_uf ~build:(fun uf solver ->
    let x = Uf.new_const uf in
    let y = Uf.new_const uf in
    let z = Uf.new_const uf in
    ignore (Uf.register_eq uf ~var:1 ~lhs:x ~rhs:y : int);
    ignore (Uf.register_neq uf ~var:2 ~lhs:y ~rhs:z : int);
    ignore (Solver.add_clause solver ~clause:[| 1 |] : [ `Ok | `Unsat of _ ]);
    ignore (Solver.add_clause solver ~clause:[| 2 |] : [ `Ok | `Unsat of _ ]));
  [%expect {| (Sat (assignments (() (true) (true)))) |}]
;;

(* ───────────────────────────────────────────────────────────────────────── *)

(* Congruence conflict: the Boolean solver sees vars 1 (x=y) and 3 (f(x)≠f(y))
   as independent and would happily set both true. The theory must detect that
   x=y implies f(x)=f(y), conflicting with f(x)≠f(y).
   We also register var 2 for f(x)=f(y) so the theory can reference it in the
   conflict clause. The boolean formula is satisfiable without the theory. *)
let%expect_test "congruence conflict is unsat" =
  solve_with_uf ~build:(fun uf solver ->
    let x = Uf.new_const uf in
    let y = Uf.new_const uf in
    let fx = Uf.new_app uf ~func:0 ~args:[| x |] in
    let fy = Uf.new_app uf ~func:0 ~args:[| y |] in
    ignore (Uf.register_eq uf ~var:1 ~lhs:x ~rhs:y : int);
    ignore (Uf.register_eq uf ~var:2 ~lhs:fx ~rhs:fy : int);
    ignore (Uf.register_neq uf ~var:3 ~lhs:fx ~rhs:fy : int);
    (* Force x=y and f(x)≠f(y) — boolean-SAT, theory-UNSAT *)
    ignore (Solver.add_clause solver ~clause:[| 1 |] : [ `Ok | `Unsat of _ ]);
    ignore (Solver.add_clause solver ~clause:[| 3 |] : [ `Ok | `Unsat of _ ]));
  [%expect {| (Unsat (unsat_core (1))) |}]
;;

(* ───────────────────────────────────────────────────────────────────────── *)

(* Theory pop test: the formula has two choices — eq_xy or eq_yz must hold,
   plus neq_xz must hold. If the solver tries eq_xy=true first, the theory
   asserts x=y; then neq_xz=true asserts x≠z — so far consistent. Then
   eq_yz is unconstrained. No conflict. If instead it tries eq_xy=true and
   eq_yz=true together with neq_xz, the theory finds x=y=z but x≠z:
   conflict, backtrack. After pop, the theory must correctly forget x=y,
   so the final SAT assignment (eq_xy=false, eq_yz=true, neq_xz=true) is
   consistent: y=z but x≠z is fine. *)
let%expect_test "theory pop: backtrack restores consistency" =
  solve_with_uf ~build:(fun uf solver ->
    let x = Uf.new_const uf in
    let y = Uf.new_const uf in
    let z = Uf.new_const uf in
    ignore (Uf.register_eq uf ~var:1 ~lhs:x ~rhs:y : int);
    ignore (Uf.register_eq uf ~var:2 ~lhs:y ~rhs:z : int);
    ignore (Uf.register_neq uf ~var:3 ~lhs:x ~rhs:z : int);
    (* At least one of eq_xy or eq_yz must hold *)
    ignore (Solver.add_clause solver ~clause:[| 1; 2 |] : [ `Ok | `Unsat of _ ]);
    (* neq_xz must hold *)
    ignore (Solver.add_clause solver ~clause:[| 3 |] : [ `Ok | `Unsat of _ ]));
  [%expect {| (Sat (assignments (() (false) (true) (true)))) |}]
;;

(* ───────────────────────────────────────────────────────────────────────── *)

(* Congruence propagation: x=y is asserted, the theory propagates f(x)=f(y)
   (var 2) as a T-consequence. The Boolean clause ¬(f(x)=f(y)) then conflicts
   with the propagated literal, giving UNSAT. The boolean formula alone
   (var 1 = true, var 2 = false) is satisfiable. *)
let%expect_test "congruence propagation triggers conflict" =
  solve_with_uf ~build:(fun uf solver ->
    let x = Uf.new_const uf in
    let y = Uf.new_const uf in
    let fx = Uf.new_app uf ~func:0 ~args:[| x |] in
    let fy = Uf.new_app uf ~func:0 ~args:[| y |] in
    ignore (Uf.register_eq uf ~var:1 ~lhs:x ~rhs:y : int);
    ignore (Uf.register_eq uf ~var:2 ~lhs:fx ~rhs:fy : int);
    (* x=y must hold *)
    ignore (Solver.add_clause solver ~clause:[| 1 |] : [ `Ok | `Unsat of _ ]);
    (* f(x)=f(y) must not hold — conflicts with theory propagation *)
    ignore (Solver.add_clause solver ~clause:[| -2 |] : [ `Ok | `Unsat of _ ]));
  [%expect {| (Unsat (unsat_core (1))) |}]
;;
