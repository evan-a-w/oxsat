open! Core
open! Feel.Import
open! Theory_core
open! Theory

let q = Formula.widen_quantified
let q_triggers = List.map ~f:(List.map ~f:q)
let xg : Formula.any = Var (Tvar.of_string "x")
let fg arg : Formula.any = App (Tvar.of_string "f", [ arg ])
let a : Formula.quantified = Var (Tvar.of_string "a")

let forall_raw (bound, triggers, body) : Formula.quantified =
  Forall (bound, q_triggers triggers, q body)
;;

let exists_raw (bound, body) : Formula.quantified = Exists (bound, q body)

let forall ?(triggers = [ [ fg xg ] ]) body : Formula.quantified =
  forall_raw ([ Tvar.of_string "x" ], triggers, body)
;;

let exists body : Formula.quantified = exists_raw ([ Tvar.of_string "x" ], body)

let print_elaborate (formula : Formula.quantified) =
  let ground, axioms = Quantifier_elaboration.elaborate formula in
  print_s
    [%message
      "" (ground : Formula.any) (axioms : Quantifier_axiom.Axiom.t list)]
;;

let%expect_test "top-level forall: registers an axiom, ground is its guard" =
  print_elaborate (forall (Eq (fg xg, xg)));
  [%expect
    {|
    ((ground (Eq (Var %guard.2) (Var %guard.1)))
     (axioms
      (((guard ((Eq (Var %guard.2) (Var %guard.1)))) (bound (x.bound.0))
        (triggers (((App f ((Var x.bound.0))))))
        (body (Eq (App f ((Var x.bound.0))) (Var x.bound.0)))))))
    |}]
;;

let%expect_test "top-level exists: Skolemized eagerly, no axiom" =
  print_elaborate (exists (Eq (fg xg, xg)));
  [%expect
    {| ((ground (Eq (App f ((Var %skolem.3))) (Var %skolem.3))) (axioms ())) |}]
;;

let%expect_test "negated forall becomes an exists (Skolemized), no axiom" =
  print_elaborate (Not (forall (Eq (fg xg, xg))));
  [%expect
    {| ((ground (Not (Eq (App f ((Var %skolem.4))) (Var %skolem.4)))) (axioms ())) |}]
;;

let%expect_test "negated exists becomes a triggerless (inert) forall" =
  print_elaborate (Not (exists (Eq (fg xg, xg))));
  [%expect
    {|
    ((ground (Eq (Var %guard.7) (Var %guard.6)))
     (axioms
      (((guard ((Eq (Var %guard.7) (Var %guard.6)))) (bound (x.bound.5))
        (triggers ())
        (body (Not (Eq (App f ((Var x.bound.5))) (Var x.bound.5))))))))
    |}]
;;

let%expect_test "forall nested under Or: guard spliced in place, axiom still \
                 registered"
  =
  print_elaborate (Or [ Eq (a, a); forall (Eq (fg xg, xg)) ]);
  [%expect
    {|
    ((ground (Or ((Eq (Var a) (Var a)) (Eq (Var %guard.10) (Var %guard.9)))))
     (axioms
      (((guard ((Eq (Var %guard.10) (Var %guard.9)))) (bound (x.bound.8))
        (triggers (((App f ((Var x.bound.8))))))
        (body (Eq (App f ((Var x.bound.8))) (Var x.bound.8)))))))
    |}]
;;

let%expect_test "two independent foralls reusing the same bound-variable name \
                 don't collide"
  =
  print_elaborate
    (And [ forall (Eq (fg xg, xg)); forall (Not (Eq (fg xg, xg))) ]);
  [%expect
    {|
    ((ground
      (And
       ((Eq (Var %guard.13) (Var %guard.12))
        (Eq (Var %guard.16) (Var %guard.15)))))
     (axioms
      (((guard ((Eq (Var %guard.13) (Var %guard.12)))) (bound (x.bound.11))
        (triggers (((App f ((Var x.bound.11))))))
        (body (Eq (App f ((Var x.bound.11))) (Var x.bound.11))))
       ((guard ((Eq (Var %guard.16) (Var %guard.15)))) (bound (x.bound.14))
        (triggers (((App f ((Var x.bound.14))))))
        (body (Not (Eq (App f ((Var x.bound.14))) (Var x.bound.14))))))))
    |}]
;;

(* ----- Manual (non-looping) instantiation against a hand-built egraph -----
   Exercises the trigger -> [Pattern.Query.t] bridge and substitution-back-into
   -body path directly, ahead of wiring the automatic solve loop. *)

let instantiate_once
  (axiom : Quantifier_axiom.Axiom.t)
  (uf : Formula_egraph_uf.t)
  : Formula.any list
  =
  let graph = Formula_egraph_uf.egraph uf in
  List.concat_map axiom.triggers ~f:(fun trigger ->
    match trigger with
    | [ term ] ->
      let query = Quantifier_axiom.query_of_term ~bound:axiom.bound term in
      Formula_egraph.Pattern.Query.search query ~graph
      |> List.filter_map ~f:(fun m ->
        Quantifier_axiom.substitution_of_match uf ~bound:axiom.bound m
        |> Option.map ~f:(fun subst -> Formula.substitute subst axiom.body))
    | _ -> failwith "test only exercises single-term triggers")
;;

let print_elaborate_and_instances formula ~seeds =
  let ground, axioms = Quantifier_elaboration.elaborate formula in
  let uf = Formula_egraph_uf.create ~atoms:[] in
  List.iter seeds ~f:(fun term -> Formula_egraph_uf.add_term uf ~term);
  let instances =
    List.concat_map axioms ~f:(fun axiom -> instantiate_once axiom uf)
  in
  print_s
    [%message
      ""
        (ground : Formula.any)
        (axioms : Quantifier_axiom.Axiom.t list)
        (instances : Formula.any list)]
;;

let%expect_test "manual instantiate-once: single ground match" =
  let x = Tvar.of_string "x" in
  let f_sym = Tvar.of_string "f" in
  let fx : Formula.any = App (f_sym, [ Var x ]) in
  let axiom : Quantifier_axiom.Axiom.t =
    { guard = None
    ; bound = [ x ]
    ; triggers = [ [ fx ] ]
    ; body = Eq (fx, Var x)
    }
  in
  let uf = Formula_egraph_uf.create ~atoms:[] in
  let fa : Formula.any = App (f_sym, [ Var (Tvar.of_string "a") ]) in
  Formula_egraph_uf.add_term uf ~term:fa;
  print_s [%sexp (instantiate_once axiom uf : Formula.any list)];
  [%expect {| ((Eq (App f ((Var a))) (Var a))) |}]
;;

let%expect_test "manual instantiate-once: two ground matches, no matches for \
                 an unrelated symbol"
  =
  let x = Tvar.of_string "x" in
  let f_sym = Tvar.of_string "f" in
  let fx : Formula.any = App (f_sym, [ Var x ]) in
  let axiom : Quantifier_axiom.Axiom.t =
    { guard = None
    ; bound = [ x ]
    ; triggers = [ [ fx ] ]
    ; body = Eq (fx, Var x)
    }
  in
  let uf = Formula_egraph_uf.create ~atoms:[] in
  Formula_egraph_uf.add_term
    uf
    ~term:(App (f_sym, [ Var (Tvar.of_string "a") ]));
  Formula_egraph_uf.add_term
    uf
    ~term:(App (f_sym, [ Var (Tvar.of_string "b") ]));
  Formula_egraph_uf.add_term
    uf
    ~term:(App (Tvar.of_string "g", [ Var (Tvar.of_string "c") ]));
  print_s [%sexp (instantiate_once axiom uf : Formula.any list)];
  [%expect
    {| ((Eq (App f ((Var a))) (Var a)) (Eq (App f ((Var b))) (Var b))) |}]
;;

(* ----- Automatic solve loop (Quantifier_solver) ----- *)

let forall_axiom
  ~(trigger : Formula.any -> Formula.any)
  ~(body : Formula.any -> Formula.any)
  : Formula.quantified
  =
  let x = Tvar.of_string "x" in
  let xv : Formula.any = Var x in
  forall_raw ([ x ], [ [ trigger xv ] ], body xv)
;;

let%expect_test "forall x. f x = x contradicts an asserted disequality, only \
                 after instantiation"
  =
  let qs = Quantifier_solver.create () in
  let f_sym = Tvar.of_string "f" in
  let f arg : Formula.any = App (f_sym, [ arg ]) in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (forall_axiom ~trigger:f ~body:(fun x -> Eq (f x, x)))
     : _ Or_error.t);
  let a : Formula.any = Var (Tvar.of_string "a") in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (Formula.widen_quantified (Not (Eq (f a, a))))
     : _ Or_error.t);
  print_s [%sexp (Quantifier_solver.solve qs : Quantifier_solver.Result.t)];
  [%expect
    {|
    (Unsat
     (core
      ((Quantifier_instance
        (body (Eq (App f ((Var x.bound.17))) (Var x.bound.17)))
        (bound_values ((x.bound.17 (Var a))))
        (instance (Eq (App f ((Var a))) (Var a))))
       (Asserted (Not (Eq (App f ((Var a))) (Var a)))))))
    |}]
;;

let%expect_test "a purely ground problem (no axioms) still returns a definite \
                 Sat"
  =
  let qs = Quantifier_solver.create () in
  let a : Formula.any = Var (Tvar.of_string "a") in
  ignore
    (Quantifier_solver.assert_formula qs (Formula.widen_quantified (Eq (a, a)))
     : _ Or_error.t);
  (match Quantifier_solver.solve qs with
   | Sat _ -> print_endline "Sat"
   | Unknown_but_possibly_sat _ -> print_endline "unexpected Unknown"
   | Unsat _ as r -> print_s [%sexp (r : Quantifier_solver.Result.t)]);
  [%expect {| Sat |}]
;;

(* With a universal axiom in play, a saturated ground model is only
   [Unknown_but_possibly_sat]: nothing contradicts [forall x. f x = x], but
   trigger-based instantiation can't certify it over terms no trigger reached. *)
let%expect_test "forall x. f x = x with no contradiction is only possibly-sat" =
  let qs = Quantifier_solver.create () in
  let f_sym = Tvar.of_string "f" in
  let f arg : Formula.any = App (f_sym, [ arg ]) in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (forall_axiom ~trigger:f ~body:(fun x -> Eq (f x, x)))
     : _ Or_error.t);
  let a : Formula.any = Var (Tvar.of_string "a") in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (Formula.widen_quantified (Eq (f a, f a)))
     : _ Or_error.t);
  (match Quantifier_solver.solve qs with
   | Sat _ -> print_endline "unexpected definite Sat"
   | Unknown_but_possibly_sat _ -> print_endline "Unknown_but_possibly_sat"
   | Unsat _ as r -> print_s [%sexp (r : Quantifier_solver.Result.t)]);
  [%expect {| Unknown_but_possibly_sat |}]
;;

let%expect_test "max_rounds bounds an axiom whose instances keep matching \
                 their own trigger"
  =
  (* forall x. f (f x) = f x, triggered by [f x]: instantiating on [f a] asserts
     [f (f a) = f a], which registers the new ground term [f (f a)] -- itself
     shaped like [f x], so it matches again on the next round, forever. *)
  let qs = Quantifier_solver.create () in
  let f_sym = Tvar.of_string "f" in
  let f arg : Formula.any = App (f_sym, [ arg ]) in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (forall_axiom ~trigger:f ~body:(fun x -> Eq (f (f x), f x)))
     : _ Or_error.t);
  let a : Formula.any = Var (Tvar.of_string "a") in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (Formula.widen_quantified (Eq (f a, f a)))
     : _ Or_error.t);
  (match Quantifier_solver.solve qs ~max_rounds:3 with
   | Unknown_but_possibly_sat _ ->
     print_endline "Unknown_but_possibly_sat (terminated within max_rounds)"
   | Sat _ -> print_endline "unexpected definite Sat"
   | Unsat _ as r -> print_s [%sexp (r : Quantifier_solver.Result.t)]);
  [%expect {| Unknown_but_possibly_sat (terminated within max_rounds) |}];
  (* Registered terms grew (bounded by max_rounds), rather than never returning. *)
  let term_count =
    Formula_egraph_uf.registered_terms (Quantifier_solver.egraph qs)
    |> List.length
  in
  print_s [%message "" (term_count : int)];
  [%expect {| (term_count 10) |}]
;;

let%expect_test "produce_proofs: a quantifier-driven unsat yields a checked \
                 proof, with zero special-casing needed for instances"
  =
  let qs =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let f_sym = Tvar.of_string "f" in
  let f arg : Formula.any = App (f_sym, [ arg ]) in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (forall_axiom ~trigger:f ~body:(fun x -> Eq (f x, x)))
     : _ Or_error.t);
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  ignore
    (Quantifier_solver.assert_formula qs (Formula.widen_quantified (Eq (a, b)))
     : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       qs
       (Formula.widen_quantified (Not (Eq (f a, b))))
     : _ Or_error.t);
  (match Quantifier_solver.solve qs ~max_rounds:2 with
   | Sat _ | Unknown_but_possibly_sat _ -> print_endline "unexpected sat"
   | Unsat { proof = None; _ } -> print_endline "no proof produced"
   | Unsat { proof = Some proof; _ } ->
     print_s [%message "" ~checked:(Or_error.is_ok (Proof.check proof) : bool)];
     print_endline (Proof.to_string_hum proof));
  [%expect
    {|
    (checked true)
    Assumptions:
      a0: ∀x.bound.20. f(x.bound.20) = x.bound.20
      a1: bool ≠ int
      a2: bool ≠ float
      a3: int ≠ float
      a4: a = b
      a5: f(a) ≠ b
    Steps:
      s0: ∀x.bound.20. f(x.bound.20) = x.bound.20   [assumption a0]
      s1: bool ≠ int   [assumption a1]
      s2: bool ≠ float   [assumption a2]
      s3: int ≠ float   [assumption a3]
      s4: a = b   [assumption a4]
      s5: f(a) ≠ b   [assumption a5]
      s6: f(a) = a   [∀-instantiation {x.bound.20 := a} over [s0]]
      s7: false   [refutation of [s1, s2, s3, s4, s5, s6]]
        refutation:
          steps:
            r0: a = b   [s4]
            r1: b ≠ f(a)   [s5]
            r2: a = f(a)   [s6]
            r3: a ≠ b ∨ a ≠ f(a) ∨ b = f(a)   [EUF: b = f(a) via [a = b; a = f(a)]]
            r4: ⊥   [RUP over [r0, r1, r2, r3]]
    Conclusion: s7
    |}]
;;

(* A bare top-level [∃] on its own, driven to unsat with no universal involved:
   [∃x. f(x) = a ∧ g(x) = a ∧ f(x) ≠ g(x)] Skolemizes to a body whose three
   literals have no direct clash -- EUF must derive [f(sk) = g(sk)] from the two
   equalities to contradict the disequality. The proof cites the real [∃] and
   justifies its Skolem body with a checked [∃-elimination] step. *)
let%expect_test "produce_proofs: a bare existential drives a checked, fully \
                 printed proof"
  =
  let qs =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let f arg : Formula.any = App (Tvar.of_string "f", [ arg ]) in
  let g arg : Formula.any = App (Tvar.of_string "g", [ arg ]) in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let x = Tvar.of_string "x" in
  let existential : Formula.quantified =
    exists_raw
      ( [ x ]
      , And
          [ Eq (f (Var x), a)
          ; Eq (g (Var x), a)
          ; Not (Eq (f (Var x), g (Var x)))
          ] )
  in
  ignore (Quantifier_solver.assert_formula qs existential : _ Or_error.t);
  (match Quantifier_solver.solve qs ~max_rounds:2 with
   | Sat _ | Unknown_but_possibly_sat _ -> print_endline "unexpected sat"
   | Unsat { proof = None; _ } -> print_endline "no proof produced"
   | Unsat { proof = Some proof; _ } ->
     print_s [%message "" ~checked:(Or_error.is_ok (Proof.check proof) : bool)];
     print_endline (Proof.to_string_hum proof));
  [%expect
    {|
    (checked true)
    Assumptions:
      a0: ∃x. f(x) = a ∧ g(x) = a ∧ f(x) ≠ g(x)
      a1: bool ≠ int
      a2: bool ≠ float
      a3: int ≠ float
    Steps:
      s0: ∃x. f(x) = a ∧ g(x) = a ∧ f(x) ≠ g(x)   [assumption a0]
      s1: bool ≠ int   [assumption a1]
      s2: bool ≠ float   [assumption a2]
      s3: int ≠ float   [assumption a3]
      s4: f(%skolem.21) = a ∧ g(%skolem.21) = a ∧ f(%skolem.21) ≠ g(%skolem.21)   [∃-elimination {x := %skolem.21} over [s0]]
      s5: false   [refutation of [s1, s2, s3, s4]]
        refutation:
          extensions:
            e0 := (a = f(%skolem.21) ∧ a = g(%skolem.21) ∧ ¬(f(%skolem.21) = g(%skolem.21)))
          steps:
            r0: a = f(%skolem.21) ∨ ¬(e0)   [definition of e0]
            r1: a = g(%skolem.21) ∨ ¬(e0)   [definition of e0]
            r2: f(%skolem.21) ≠ g(%skolem.21) ∨ ¬(e0)   [definition of e0]
            r3: e0   [s4]
            r4: a ≠ f(%skolem.21) ∨ a ≠ g(%skolem.21) ∨ f(%skolem.21) = g(%skolem.21)   [EUF: f(%skolem.21) = g(%skolem.21) via [a = f(%skolem.21); a = g(%skolem.21)]]
            r5: ⊥   [RUP over [r3, r0, r1, r2, r4]]
    Conclusion: s5
    |}]
;;

(* A top-level conjunction of a universal and an existential -- the user's
   "forall and existential in the same formula, at toplevel" case. Each conjunct
   is handled at top level, so both get real proof steps: the [∃] is eliminated
   to a Skolem body [f(sk) = c ∧ sk ≠ c], the [∀] instantiates on [f(sk)] to
   give [f(sk) = sk], and EUF (rather than a direct literal clash) closes it. *)
let%expect_test "produce_proofs: forall + existential in one top-level \
                 conjunction, both cited"
  =
  let qs =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let f_sym = Tvar.of_string "f" in
  let f arg : Formula.any = App (f_sym, [ arg ]) in
  let c : Formula.any = Var (Tvar.of_string "c") in
  let y = Tvar.of_string "y" in
  let x = Tvar.of_string "x" in
  let forall : Formula.quantified =
    forall_raw ([ y ], [ [ f (Var y) ] ], Eq (f (Var y), Var y))
  in
  let exists : Formula.quantified =
    exists_raw ([ x ], And [ Eq (f (Var x), c); Not (Eq (Var x, c)) ])
  in
  ignore
    (Quantifier_solver.assert_formula qs (And [ forall; exists ])
     : _ Or_error.t);
  (match Quantifier_solver.solve qs ~max_rounds:2 with
   | Sat _ | Unknown_but_possibly_sat _ -> print_endline "unexpected sat"
   | Unsat { proof = None; _ } -> print_endline "no proof produced"
   | Unsat { proof = Some proof; _ } ->
     print_s [%message "" ~checked:(Or_error.is_ok (Proof.check proof) : bool)];
     print_endline (Proof.to_string_hum proof));
  [%expect
    {|
    (checked true)
    Assumptions:
      a0: ∀y.bound.22. f(y.bound.22) = y.bound.22
      a1: ∃x. f(x) = c ∧ x ≠ c
      a2: bool ≠ int
      a3: bool ≠ float
      a4: int ≠ float
    Steps:
      s0: ∀y.bound.22. f(y.bound.22) = y.bound.22   [assumption a0]
      s1: ∃x. f(x) = c ∧ x ≠ c   [assumption a1]
      s2: bool ≠ int   [assumption a2]
      s3: bool ≠ float   [assumption a3]
      s4: int ≠ float   [assumption a4]
      s5: f(%skolem.23) = c ∧ %skolem.23 ≠ c   [∃-elimination {x := %skolem.23} over [s1]]
      s6: f(%skolem.23) = %skolem.23   [∀-instantiation {y.bound.22 := %skolem.23} over [s0]]
      s7: false   [refutation of [s2, s3, s4, s5, s6]]
        refutation:
          extensions:
            e0 := (c = f(%skolem.23) ∧ ¬(c = %skolem.23))
          steps:
            r0: c = f(%skolem.23) ∨ ¬(e0)   [definition of e0]
            r1: c ≠ %skolem.23 ∨ ¬(e0)   [definition of e0]
            r2: e0   [s5]
            r3: %skolem.23 = f(%skolem.23)   [s6]
            r4: c = %skolem.23 ∨ c ≠ f(%skolem.23) ∨ %skolem.23 ≠ f(%skolem.23)   [EUF: c = %skolem.23 via [c = f(%skolem.23); %skolem.23 = f(%skolem.23)]]
            r5: ⊥   [RUP over [r2, r3, r0, r1, r4]]
    Conclusion: s7
    |}]
;;

let print_checked_proof = function
  | Quantifier_solver.Result.Sat _ | Unknown_but_possibly_sat _ ->
    print_endline "unexpected sat"
  | Unsat { proof = None; _ } -> print_endline "no proof produced"
  | Unsat { proof = Some proof; _ } ->
    print_s [%message "" ~checked:(Or_error.is_ok (Proof.check proof) : bool)];
    print_endline (Proof.to_string_hum proof)
;;

let%expect_test "produce_proofs: forall-exists alternation instantiates then \
                 eliminates"
  =
  let qs =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let f x : Formula.any = App (Tvar.of_string "f", [ x ]) in
  let h x : Formula.any = App (Tvar.of_string "h", [ x ]) in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (forall_raw
          ( [ x ]
          , [ [ h (Var x) ] ]
          , Exists
              ( [ y ]
              , Formula.widen_quantified
                  (And [ Eq (f (Var x), Var y); Not (Eq (f (Var x), Var y)) ])
              ) ))
     : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       qs
       (Formula.widen_quantified (Eq (h a, h a)))
     : _ Or_error.t);
  print_checked_proof (Quantifier_solver.solve qs ~max_rounds:2);
  [%expect
    {|
    (checked true)
    Assumptions:
      a0: ∀x.bound.24. ∃y.bound.25. f(x.bound.24) = y.bound.25 ∧ f(x.bound.24) ≠ y.bound.25
      a1: bool ≠ int
      a2: bool ≠ float
      a3: int ≠ float
      a4: h(a) = h(a)
    Steps:
      s0: ∀x.bound.24. ∃y.bound.25. f(x.bound.24) = y.bound.25 ∧ f(x.bound.24) ≠ y.bound.25   [assumption a0]
      s1: bool ≠ int   [assumption a1]
      s2: bool ≠ float   [assumption a2]
      s3: int ≠ float   [assumption a3]
      s4: h(a) = h(a)   [assumption a4]
      s5: ∃y.bound.25. f(a) = y.bound.25 ∧ f(a) ≠ y.bound.25   [∀-instantiation {x.bound.24 := a} over [s0]]
      s6: f(a) = %skolem.27 ∧ f(a) ≠ %skolem.27   [∃-elimination {y.bound.25 := %skolem.27} over [s5]]
      s7: false   [refutation of [s1, s2, s3, s4, s6]]
        refutation:
          extensions:
            e0 := (%skolem.27 = f(a) ∧ ¬(%skolem.27 = f(a)))
          steps:
            r0: %skolem.27 = f(a) ∨ ¬(e0)   [definition of e0]
            r1: %skolem.27 ≠ f(a) ∨ ¬(e0)   [definition of e0]
            r2: e0   [s6]
            r3: ⊥   [RUP over [r2, r0, r1]]
    Conclusion: s7
    |}]
;;

let%expect_test "produce_proofs: exists-forall alternation eliminates then \
                 instantiates"
  =
  let qs =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  let f x : Formula.any = App (Tvar.of_string "f", [ x ]) in
  let h x : Formula.any = App (Tvar.of_string "h", [ x ]) in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (Exists
          ( [ x ]
          , Forall
              ( [ y ]
              , [ [ Formula.widen_quantified (h (Var y)) ] ]
              , Formula.widen_quantified (Eq (f (Var y), Var x)) ) ))
     : _ Or_error.t);
  List.iter
    [ Formula.widen_quantified (Eq (h a, h a))
    ; Formula.widen_quantified (Eq (h b, h b))
    ; Formula.widen_quantified (Not (Eq (f a, f b)))
    ]
    ~f:(fun formula ->
      ignore (Quantifier_solver.assert_formula qs formula : _ Or_error.t));
  print_checked_proof (Quantifier_solver.solve qs ~max_rounds:3);
  [%expect
    {|
    (checked true)
    Assumptions:
      a0: ∃x.bound.28. ∀y.bound.30. f(y.bound.30) = x.bound.28
      a1: bool ≠ int
      a2: bool ≠ float
      a3: int ≠ float
      a4: h(a) = h(a)
      a5: h(b) = h(b)
      a6: f(a) ≠ f(b)
    Steps:
      s0: ∃x.bound.28. ∀y.bound.30. f(y.bound.30) = x.bound.28   [assumption a0]
      s1: bool ≠ int   [assumption a1]
      s2: bool ≠ float   [assumption a2]
      s3: int ≠ float   [assumption a3]
      s4: h(a) = h(a)   [assumption a4]
      s5: h(b) = h(b)   [assumption a5]
      s6: f(a) ≠ f(b)   [assumption a6]
      s7: ∀y.bound.30. f(y.bound.30) = %skolem.31   [∃-elimination {x.bound.28 := %skolem.31} over [s0]]
      s8: f(a) = %skolem.31   [∀-instantiation {y.bound.30 := a} over [s7]]
      s9: f(b) = %skolem.31   [∀-instantiation {y.bound.30 := b} over [s7]]
      s10: false   [refutation of [s1, s2, s3, s4, s5, s6, s8, s9]]
        refutation:
          steps:
            r0: f(a) ≠ f(b)   [s6]
            r1: %skolem.31 = f(a)   [s8]
            r2: %skolem.31 = f(b)   [s9]
            r3: %skolem.31 ≠ f(a) ∨ %skolem.31 ≠ f(b) ∨ f(a) = f(b)   [EUF: f(a) = f(b) via [%skolem.31 = f(a); %skolem.31 = f(b)]]
            r4: ⊥   [RUP over [r0, r1, r2, r3]]
    Conclusion: s10
    |}]
;;

let%expect_test "produce_proofs: forall-exists uses distinct witnesses at two \
                 terms"
  =
  let qs =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  let c : Formula.any = Var (Tvar.of_string "c") in
  let f x : Formula.any = App (Tvar.of_string "f", [ x ]) in
  let h x : Formula.any = App (Tvar.of_string "h", [ x ]) in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (forall_raw
          ( [ x ]
          , [ [ h (Var x) ] ]
          , Exists
              ( [ y ]
              , Formula.widen_quantified
                  (And [ Eq (f (Var x), Var y); Eq (Var y, c) ]) ) ))
     : _ Or_error.t);
  List.iter
    [ Formula.widen_quantified (Eq (h a, h a))
    ; Formula.widen_quantified (Eq (h b, h b))
    ; Formula.widen_quantified (Not (Eq (f a, f b)))
    ]
    ~f:(fun formula ->
      ignore (Quantifier_solver.assert_formula qs formula : _ Or_error.t));
  print_checked_proof (Quantifier_solver.solve qs ~max_rounds:3);
  [%expect
    {|
    (checked true)
    Assumptions:
      a0: ∀x.bound.32. ∃y.bound.33. f(x.bound.32) = y.bound.33 ∧ y.bound.33 = c
      a1: bool ≠ int
      a2: bool ≠ float
      a3: int ≠ float
      a4: h(a) = h(a)
      a5: h(b) = h(b)
      a6: f(a) ≠ f(b)
    Steps:
      s0: ∀x.bound.32. ∃y.bound.33. f(x.bound.32) = y.bound.33 ∧ y.bound.33 = c   [assumption a0]
      s1: bool ≠ int   [assumption a1]
      s2: bool ≠ float   [assumption a2]
      s3: int ≠ float   [assumption a3]
      s4: h(a) = h(a)   [assumption a4]
      s5: h(b) = h(b)   [assumption a5]
      s6: f(a) ≠ f(b)   [assumption a6]
      s7: ∃y.bound.33. f(a) = y.bound.33 ∧ y.bound.33 = c   [∀-instantiation {x.bound.32 := a} over [s0]]
      s8: f(a) = %skolem.35 ∧ %skolem.35 = c   [∃-elimination {y.bound.33 := %skolem.35} over [s7]]
      s9: ∃y.bound.33. f(b) = y.bound.33 ∧ y.bound.33 = c   [∀-instantiation {x.bound.32 := b} over [s0]]
      s10: f(b) = %skolem.36 ∧ %skolem.36 = c   [∃-elimination {y.bound.33 := %skolem.36} over [s9]]
      s11: false   [refutation of [s1, s2, s3, s4, s5, s6, s8, s10]]
        refutation:
          extensions:
            e0 := (%skolem.35 = f(a) ∧ c = %skolem.35)
            e1 := (%skolem.36 = f(b) ∧ c = %skolem.36)
          steps:
            r0: f(a) ≠ f(b)   [s6]
            r1: %skolem.35 = f(a) ∨ ¬(e0)   [definition of e0]
            r2: c = %skolem.35 ∨ ¬(e0)   [definition of e0]
            r3: e0   [s8]
            r4: %skolem.36 = f(b) ∨ ¬(e1)   [definition of e1]
            r5: c = %skolem.36 ∨ ¬(e1)   [definition of e1]
            r6: e1   [s10]
            r7: c ≠ %skolem.35 ∨ c ≠ %skolem.36 ∨ %skolem.35 ≠ f(a) ∨ %skolem.36 ≠ f(b) ∨ f(a) = f(b)   [EUF: f(a) = f(b) via [%skolem.35 = f(a); c = %skolem.35; c = %skolem.36; %skolem.36 = f(b)]]
            r8: ⊥   [RUP over [r0, r3, r6, r1, r2, r4, r5, r7]]
    Conclusion: s11
    |}]
;;

let%expect_test "produce_proofs: alternating proof rejects reused existential \
                 witness"
  =
  let qs =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  let c : Formula.any = Var (Tvar.of_string "c") in
  let f x : Formula.any = App (Tvar.of_string "f", [ x ]) in
  let h x : Formula.any = App (Tvar.of_string "h", [ x ]) in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (forall_raw
          ( [ x ]
          , [ [ h (Var x) ] ]
          , Exists
              ( [ y ]
              , Formula.widen_quantified
                  (And [ Eq (f (Var x), Var y); Eq (Var y, c) ]) ) ))
     : _ Or_error.t);
  List.iter
    [ Formula.widen_quantified (Eq (h a, h a))
    ; Formula.widen_quantified (Eq (h b, h b))
    ; Formula.widen_quantified (Not (Eq (f a, f b)))
    ]
    ~f:(fun formula ->
      ignore (Quantifier_solver.assert_formula qs formula : _ Or_error.t));
  (match Quantifier_solver.solve qs ~max_rounds:3 with
   | Sat _ | Unknown_but_possibly_sat _ -> print_endline "unexpected sat"
   | Unsat { proof = None; _ } -> print_endline "no proof produced"
   | Unsat { proof = Some proof; _ } ->
     let exists_steps =
       Array.filter_mapi proof.steps ~f:(fun index step ->
         match step.Proof.Step.justification with
         | Kernel { rule = Exists_elim { skolems }; _ } -> Some (index, skolems)
         | _ -> None)
     in
     let reused = snd exists_steps.(0) in
     let _second_index, second_skolems = exists_steps.(1) in
     let subst =
       List.map2_exn
         second_skolems
         reused
         ~f:(fun (_bound, old_) (_bound, new_) ->
           match old_ with
           | Var old_var -> old_var, new_
           | _ -> failwith "test expected a variable Skolem")
       |> Tvar.Map.of_alist_exn
     in
     let mutated =
       { proof with
         steps =
           Array.mapi proof.steps ~f:(fun index step ->
             if index = fst exists_steps.(1)
             then (
               match step.justification with
               | Kernel { premises; rule = Exists_elim _ } ->
                 { step with
                   conclusion =
                     Or_error.ok_exn
                       (Formula.substitute_quantified subst step.conclusion)
                 ; justification =
                     Kernel
                       { rule = Exists_elim { skolems = reused }; premises }
                 }
               | _ -> step)
             else step)
       }
     in
     print_s [%sexp (Proof.check mutated : unit Or_error.t)]);
  [%expect
    {|
    (Error
     ("existential elimination's Skolem symbol was reused for a different conclusion"
      (tvar %skolem.40)
      (previous
       (And
        ((Eq (App f ((Var a))) (Var %skolem.40)) (Eq (Var %skolem.40) (Var c)))))
      (current
       (And
        ((Eq (App f ((Var b))) (Var %skolem.40)) (Eq (Var %skolem.40) (Var c)))))))
    |}]
;;

(* A quantifier nested inside boolean structure keeps the guard encoding, whose
   guard atom is synthetic -- so a refutation depending on it declines to
   produce a real proof (the documented fallback), while still solving. *)
let%expect_test "produce_proofs: a nested quantifier still solves but declines \
                 a proof"
  =
  let qs =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let f_sym = Tvar.of_string "f" in
  let f arg : Formula.any = App (f_sym, [ arg ]) in
  let x = Tvar.of_string "x" in
  let c : Formula.any = Var (Tvar.of_string "c") in
  let d : Formula.any = Var (Tvar.of_string "d") in
  let nested_forall : Formula.quantified =
    Or
      [ Formula.widen_quantified (Formula.Eq (c, d))
      ; forall_raw ([ x ], [ [ f (Var x) ] ], Eq (f (Var x), Var x))
      ]
  in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  List.iter
    [ nested_forall
    ; Formula.widen_quantified (Formula.Not (Eq (c, d)))
    ; Formula.widen_quantified (Formula.Eq (a, b))
    ; Formula.widen_quantified (Formula.Not (Eq (f a, b)))
    ]
    ~f:(fun formula ->
      ignore (Quantifier_solver.assert_formula qs formula : _ Or_error.t));
  (match Quantifier_solver.solve qs ~max_rounds:2 with
   | Sat _ | Unknown_but_possibly_sat _ -> print_endline "unexpected sat"
   | Unsat { proof = None; _ } -> print_endline "unsat, no proof (nested)"
   | Unsat { proof = Some _; _ } -> print_endline "unexpected proof");
  [%expect {| unsat, no proof (nested) |}]
;;

(* A single universal that must be instantiated at TWO distinct, unrelated terms
   for the refutation to close: [∀x.f(x)=c] with [f(a)≠f(b)] forces both
   [f(a)=c] and [f(b)=c] (a and b share no equality, so congruence cannot bridge
   [f(a)] and [f(b)] on its own), after which EUF chains [f(a)=c=f(b)] to
   contradict the disequality. Two [∀-instantiation] steps feed one refutation. *)
let%expect_test "produce_proofs: one universal instantiated at two terms" =
  let qs =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let f arg : Formula.any = App (Tvar.of_string "f", [ arg ]) in
  let c : Formula.any = Var (Tvar.of_string "c") in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  let x = Tvar.of_string "x" in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (forall_raw ([ x ], [ [ f (Var x) ] ], Eq (f (Var x), c)))
     : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       qs
       (Formula.widen_quantified (Not (Eq (f a, f b))))
     : _ Or_error.t);
  (match Quantifier_solver.solve qs ~max_rounds:3 with
   | Sat _ | Unknown_but_possibly_sat _ -> print_endline "unexpected sat"
   | Unsat { proof = None; _ } -> print_endline "no proof produced"
   | Unsat { proof = Some proof; _ } ->
     print_s [%message "" ~checked:(Or_error.is_ok (Proof.check proof) : bool)];
     print_endline (Proof.to_string_hum proof));
  [%expect
    {|
    (checked true)
    Assumptions:
      a0: ∀x.bound.45. f(x.bound.45) = c
      a1: bool ≠ int
      a2: bool ≠ float
      a3: int ≠ float
      a4: f(a) ≠ f(b)
    Steps:
      s0: ∀x.bound.45. f(x.bound.45) = c   [assumption a0]
      s1: bool ≠ int   [assumption a1]
      s2: bool ≠ float   [assumption a2]
      s3: int ≠ float   [assumption a3]
      s4: f(a) ≠ f(b)   [assumption a4]
      s5: f(a) = c   [∀-instantiation {x.bound.45 := a} over [s0]]
      s6: f(b) = c   [∀-instantiation {x.bound.45 := b} over [s0]]
      s7: false   [refutation of [s1, s2, s3, s4, s5, s6]]
        refutation:
          steps:
            r0: f(a) ≠ f(b)   [s4]
            r1: c = f(a)   [s5]
            r2: c = f(b)   [s6]
            r3: c ≠ f(a) ∨ c ≠ f(b) ∨ f(a) = f(b)   [EUF: f(a) = f(b) via [c = f(a); c = f(b)]]
            r4: ⊥   [RUP over [r0, r1, r2, r3]]
    Conclusion: s7
    |}]
;;

(* Confidence that [Proof.check] is not vacuous: take a real, complex proof (the
   forall + existential co-occurrence) and corrupt it four ways, each of which
   the checker must reject. If any mutation were accepted, the checker would be
   trusting rather than verifying. *)
let%expect_test "produce_proofs: the checker rejects mutations of a real proof" =
  let qs =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let f arg : Formula.any = App (Tvar.of_string "f", [ arg ]) in
  let c : Formula.any = Var (Tvar.of_string "c") in
  let y = Tvar.of_string "y" in
  let x = Tvar.of_string "x" in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (And
          [ forall_raw ([ y ], [ [ f (Var y) ] ], Eq (f (Var y), Var y))
          ; exists_raw ([ x ], And [ Eq (f (Var x), c); Not (Eq (Var x, c)) ])
          ])
     : _ Or_error.t);
  match Quantifier_solver.solve qs ~max_rounds:2 with
  | Sat _ | Unknown_but_possibly_sat _ -> print_endline "unexpected sat"
  | Unsat { proof = None; _ } -> print_endline "no proof produced"
  | Unsat { proof = Some proof; _ } ->
    let find ~f =
      fst
        (Array.findi_exn proof.steps ~f:(fun _ step ->
           f step.Proof.Step.justification))
    in
    let map_step i ~f =
      { proof with
        steps =
          Array.mapi proof.steps ~f:(fun j step ->
            if j = i then f step else step)
      }
    in
    let inst_i =
      find ~f:(function
        | Proof.Justification.Kernel { rule = Forall_instantiation _; _ } ->
          true
        | _ -> false)
    in
    let exists_i =
      find ~f:(function
        | Proof.Justification.Kernel { rule = Exists_elim _; _ } -> true
        | _ -> false)
    in
    let refut_i =
      find ~f:(function
        | Proof.Justification.By_refutation _ -> true
        | _ -> false)
    in
    let bogus : Formula.any = Var (Tvar.of_string "%bogus") in
    (* 1. Change the instantiation's witness but not its conclusion. *)
    let wrong_instantiation =
      map_step inst_i ~f:(fun step ->
        match step.justification with
        | Kernel { rule = Forall_instantiation { bound_values }; premises } ->
          let bound_values =
            List.map bound_values ~f:(fun (v, _) -> v, bogus)
          in
          { step with
            justification =
              Kernel { rule = Forall_instantiation { bound_values }; premises }
          }
        | _ -> step)
    in
    (* 2. Claim the instantiation's ground conclusion is directly the [∀]
       assumption (a0). *)
    let forged_instantiation =
      map_step inst_i ~f:(fun step ->
        { step with
          justification = Assumption (Proof.Id.Assumption.of_int_exn 0)
        })
    in
    (* 3. Tamper with the witnessed body of the existential step. *)
    let tampered_exists =
      map_step exists_i ~f:(fun step ->
        { step with conclusion = Formula.widen_quantified (Eq (bogus, c)) })
    in
    (* 4. Strip the hints from the refutation's RUP step so it no longer derives
       the empty clause. *)
    let broken_refutation =
      map_step refut_i ~f:(fun step ->
        match step.justification with
        | By_refutation { premises; refutation } ->
          let steps =
            Array.map refutation.Proof.Refutation.steps ~f:(fun rstep ->
              match rstep.Proof.Refutation.Step.reason with
              | Rup _ ->
                { rstep with
                  reason = Proof.Refutation.Reason.Rup { hints = [||] }
                }
              | _ -> rstep)
          in
          { step with
            justification =
              By_refutation { premises; refutation = { refutation with steps } }
          }
        | _ -> step)
    in
    print_s
      [%message
        ""
          ~baseline_checks:(Or_error.is_ok (Proof.check proof) : bool)
          ~wrong_instantiation_rejected:
            (Or_error.is_error (Proof.check wrong_instantiation) : bool)
          ~forged_instantiation_rejected:
            (Or_error.is_error (Proof.check forged_instantiation) : bool)
          ~tampered_exists_rejected:
            (Or_error.is_error (Proof.check tampered_exists) : bool)
          ~broken_refutation_rejected:
            (Or_error.is_error (Proof.check broken_refutation) : bool)];
    [%expect
      {|
      ((baseline_checks true) (wrong_instantiation_rejected true)
       (forged_instantiation_rejected true) (tampered_exists_rejected true)
       (broken_refutation_rejected true))
      |}]
;;

(* Scope-awareness of the already-instantiated cache: an instance emitted inside
   a [push] scope must not stay suppressed after the scope is popped and its
   clause retracted. Here [f(a)=a] is emitted in-scope (against [f(a)≠c], no
   conflict); after [pop] it must be re-derivable to contradict a freshly
   asserted [f(a)≠a]. Without scope-aware [seen] the second solve would miss it
   and answer "possibly sat". *)
let%expect_test "push/pop: a popped instance is re-derivable, not suppressed" =
  let qs = Quantifier_solver.create () in
  let f arg : Formula.any = App (Tvar.of_string "f", [ arg ]) in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let c : Formula.any = Var (Tvar.of_string "c") in
  let x = Tvar.of_string "x" in
  let label = function
    | Quantifier_solver.Result.Unsat _ -> "unsat"
    | Sat _ -> "sat"
    | Unknown_but_possibly_sat _ -> "possibly-sat"
  in
  let assert_ f =
    ignore (Quantifier_solver.assert_formula qs f : _ Or_error.t)
  in
  assert_ (forall_raw ([ x ], [ [ f (Var x) ] ], Eq (f (Var x), Var x)));
  Quantifier_solver.push qs;
  assert_ (Formula.widen_quantified (Not (Eq (f a, c))));
  print_endline (label (Quantifier_solver.solve qs ~max_rounds:2));
  Quantifier_solver.pop qs;
  assert_ (Formula.widen_quantified (Not (Eq (f a, a))));
  print_endline (label (Quantifier_solver.solve qs ~max_rounds:2));
  [%expect {|
    possibly-sat
    unsat
    |}]
;;

(* An axiom registered inside a scope is dropped on [pop]: after popping the
   universal, the same disequality that was unsat with it present is now sat.
   And because no axioms remain, the result is a definite [Sat], not "possibly
   sat". *)
let%expect_test "push/pop: an axiom registered in a scope is dropped on pop" =
  let qs = Quantifier_solver.create () in
  let f arg : Formula.any = App (Tvar.of_string "f", [ arg ]) in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let x = Tvar.of_string "x" in
  let label = function
    | Quantifier_solver.Result.Unsat _ -> "unsat"
    | Sat _ -> "sat"
    | Unknown_but_possibly_sat _ -> "possibly-sat"
  in
  let assert_ f =
    ignore (Quantifier_solver.assert_formula qs f : _ Or_error.t)
  in
  Quantifier_solver.push qs;
  assert_ (forall_raw ([ x ], [ [ f (Var x) ] ], Eq (f (Var x), Var x)));
  assert_ (Formula.widen_quantified (Not (Eq (f a, a))));
  print_endline (label (Quantifier_solver.solve qs ~max_rounds:2));
  Quantifier_solver.pop qs;
  assert_ (Formula.widen_quantified (Not (Eq (f a, a))));
  print_endline (label (Quantifier_solver.solve qs ~max_rounds:2));
  [%expect {|
    unsat
    sat
    |}]
;;

let%expect_test "nested forall-exists: Skolem depends on enclosing universal" =
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let f x y : Formula.any = App (Tvar.of_string "f", [ x; y ]) in
  print_elaborate
    (forall_raw
       ( [ x ]
       , [ [ Var x ] ]
       , Exists ([ y ], Formula.widen_quantified (Eq (f (Var x) (Var y), Var x)))
       ));
  [%expect
    {|
    ((ground (Eq (Var %guard.53) (Var %guard.52)))
     (axioms
      (((guard ((Eq (Var %guard.53) (Var %guard.52)))) (bound (x.bound.50))
        (triggers (((Var x.bound.50))))
        (body
         (Eq (App f ((Var x.bound.50) (App %skolem.51 ((Var x.bound.50)))))
          (Var x.bound.50)))))))
    |}]
;;

let%expect_test "outer exists remains a ground Skolem, inner forall is hoisted" =
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let h x y : Formula.any = App (Tvar.of_string "h", [ x; y ]) in
  print_elaborate
    (Exists
       ( [ x ]
       , Forall
           ( [ y ]
           , [ [ Formula.widen_quantified (Var y) ] ]
           , Formula.widen_quantified (Eq (h (Var x) (Var y), Var y)) ) ));
  [%expect
    {|
    ((ground (Eq (Var %guard.57) (Var %guard.56)))
     (axioms
      (((guard ((Eq (Var %guard.57) (Var %guard.56)))) (bound (y.bound.55))
        (triggers (((Var y.bound.55))))
        (body (Eq (App h ((Var %skolem.54) (Var y.bound.55))) (Var y.bound.55)))))))
    |}]
;;

let%expect_test "nested foralls are prenexed into one axiom" =
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let f x y : Formula.any = App (Tvar.of_string "f", [ x; y ]) in
  print_elaborate
    (forall_raw
       ( [ x ]
       , [ [ Var x ] ]
       , Forall
           ( [ y ]
           , [ [ Formula.widen_quantified (Var y) ] ]
           , Formula.widen_quantified (Eq (f (Var x) (Var y), Var x)) ) ));
  [%expect
    {|
    ((ground (Eq (Var %guard.61) (Var %guard.60)))
     (axioms
      (((guard ((Eq (Var %guard.61) (Var %guard.60))))
        (bound (x.bound.58 y.bound.59))
        (triggers (((Var x.bound.58) (Var y.bound.59))))
        (body (Eq (App f ((Var x.bound.58) (Var y.bound.59))) (Var x.bound.58)))))))
    |}]
;;

let%expect_test "universal inside boolean structure of an axiom is hoisted" =
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let p x : Formula.any = App (Tvar.of_string "p", [ x ]) in
  let q x y : Formula.any = App (Tvar.of_string "q", [ x; y ]) in
  print_elaborate
    (forall_raw
       ( [ x ]
       , [ [ p (Var x) ] ]
       , Or
           [ Formula.widen_quantified (Eq (p (Var x), p (Var x)))
           ; Forall
               ( [ y ]
               , [ [ Formula.widen_quantified (q (Var x) (Var y)) ] ]
               , Formula.widen_quantified
                   (Eq (q (Var x) (Var y), q (Var x) (Var y))) )
           ] ));
  [%expect
    {|
    ((ground (Eq (Var %guard.65) (Var %guard.64)))
     (axioms
      (((guard ((Eq (Var %guard.65) (Var %guard.64))))
        (bound (x.bound.62 y.bound.63))
        (triggers
         (((App p ((Var x.bound.62)))
           (App q ((Var x.bound.62) (Var y.bound.63))))))
        (body
         (Or
          ((Eq (App p ((Var x.bound.62))) (App p ((Var x.bound.62))))
           (Eq (App q ((Var x.bound.62) (Var y.bound.63)))
            (App q ((Var x.bound.62) (Var y.bound.63)))))))))))
    |}]
;;

let%expect_test "hoisted trigger groups are concatenated" =
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let f x : Formula.any = App (Tvar.of_string "f", [ x ]) in
  let g y : Formula.any = App (Tvar.of_string "g", [ y ]) in
  print_elaborate
    (forall_raw
       ( [ x ]
       , [ [ f (Var x) ] ]
       , Forall
           ( [ y ]
           , [ [ Formula.widen_quantified (g (Var y)) ] ]
           , Formula.widen_quantified (Eq (f (Var x), g (Var y))) ) ));
  [%expect
    {|
    ((ground (Eq (Var %guard.69) (Var %guard.68)))
     (axioms
      (((guard ((Eq (Var %guard.69) (Var %guard.68))))
        (bound (x.bound.66 y.bound.67))
        (triggers (((App f ((Var x.bound.66))) (App g ((Var y.bound.67))))))
        (body (Eq (App f ((Var x.bound.66))) (App g ((Var y.bound.67)))))))))
    |}]
;;

let%expect_test "negative forall under universal Skolemizes with enclosing \
                 scope"
  =
  (* [y] is existential after negating the inner [forall], so its Skolem must
     depend on the enclosing universal [x]. *)
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let p x : Formula.any = App (Tvar.of_string "p", [ x ]) in
  let q x y : Formula.any = App (Tvar.of_string "q", [ x; y ]) in
  print_elaborate
    (forall_raw
       ( [ x ]
       , [ [ p (Var x) ] ]
       , Not
           (Forall
              ( [ y ]
              , [ [ Formula.widen_quantified (q (Var x) (Var y)) ] ]
              , Formula.widen_quantified (Eq (q (Var x) (Var y), Var x)) )) ));
  [%expect
    {|
    ((ground (Eq (Var %guard.73) (Var %guard.72)))
     (axioms
      (((guard ((Eq (Var %guard.73) (Var %guard.72)))) (bound (x.bound.70))
        (triggers (((App p ((Var x.bound.70))))))
        (body
         (Not
          (Eq (App q ((Var x.bound.70) (App %skolem.71 ((Var x.bound.70)))))
           (Var x.bound.70))))))))
    |}]
;;

let%expect_test "negative exists under universal hoists and scopes deeper \
                 Skolem"
  =
  (* [y] is universal after negating the inner [exists]; the deeper positive
     [exists z] must Skolemize over both [x] and the renamed [y]. *)
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let z = Tvar.of_string "z" in
  let p x : Formula.any = App (Tvar.of_string "p", [ x ]) in
  let r x y z : Formula.any = App (Tvar.of_string "r", [ x; y; z ]) in
  print_elaborate
    (forall_raw
       ( [ x ]
       , [ [ p (Var x) ] ]
       , Not
           (Exists
              ( [ y ]
              , Not
                  (Exists
                     ( [ z ]
                     , Formula.widen_quantified
                         (Eq (r (Var x) (Var y) (Var z), Var z)) )) )) ));
  [%expect
    {|
    ((ground (Eq (Var %guard.78) (Var %guard.77)))
     (axioms
      (((guard ((Eq (Var %guard.78) (Var %guard.77))))
        (bound (x.bound.74 y.bound.75)) (triggers (((App p ((Var x.bound.74))))))
        (body
         (Eq
          (App r
           ((Var x.bound.74) (Var y.bound.75)
            (App %skolem.76 ((Var x.bound.74) (Var y.bound.75)))))
          (App %skolem.76 ((Var x.bound.74) (Var y.bound.75)))))))))
    |}]
;;

let print_quantifier_result_label result =
  match (result : Quantifier_solver.Result.t) with
  | Sat _ -> print_endline "Sat"
  | Unknown_but_possibly_sat _ -> print_endline "Unknown_but_possibly_sat"
  | Unsat _ -> print_endline "Unsat"
;;

let%expect_test "soundness: exact forall x exists y. y <> x is not refuted" =
  (* The trigger and seed make this non-vacuous: the printed instance shows the
     satisfiable constraint [sk(a) <> a], with the witness depending on [x]. *)
  let qs = Quantifier_solver.create () in
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let h z : Formula.any = App (Tvar.of_string "h", [ z ]) in
  let quantified =
    forall_raw
      ( [ x ]
      , [ [ h (Var x) ] ]
      , Exists ([ y ], Formula.widen_quantified (Not (Eq (Var y, Var x)))) )
  in
  print_elaborate_and_instances quantified ~seeds:[ h a ];
  ignore (Quantifier_solver.assert_formula qs quantified : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       qs
       (Formula.widen_quantified (Eq (h a, h a)))
     : _ Or_error.t);
  print_quantifier_result_label (Quantifier_solver.solve qs ~max_rounds:5);
  [%expect
    {|
    ((ground (Eq (Var %guard.82) (Var %guard.81)))
     (axioms
      (((guard ((Eq (Var %guard.82) (Var %guard.81)))) (bound (x.bound.79))
        (triggers (((App h ((Var x.bound.79))))))
        (body (Not (Eq (App %skolem.80 ((Var x.bound.79))) (Var x.bound.79)))))))
     (instances ((Not (Eq (App %skolem.80 ((Var a))) (Var a))))))
    Unknown_but_possibly_sat
    |}]
;;

let%expect_test "soundness: triggered forall-exists does not expose c <> c" =
  (* This catches the old unsound Skolemization, which replaced [y] by one
     ground constant [c]. The tautological [h(y) = h(y)] makes [h(c)] available
     to the trigger, so the old encoding instantiated [x := c] and asserted
     [c <> c]. The fixed encoding uses [y := sk(x)], so that refutation is no
     longer possible. *)
  let qs = Quantifier_solver.create () in
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let h z : Formula.any = App (Tvar.of_string "h", [ z ]) in
  let quantified =
    forall_raw
      ( [ x ]
      , [ [ h (Var x) ] ]
      , Exists
          ( [ y ]
          , Formula.widen_quantified
              (And [ Not (Eq (Var y, Var x)); Eq (h (Var y), h (Var y)) ]) ) )
  in
  print_elaborate_and_instances quantified ~seeds:[ h a ];
  ignore (Quantifier_solver.assert_formula qs quantified : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       qs
       (Formula.widen_quantified (Eq (h a, h a)))
     : _ Or_error.t);
  print_quantifier_result_label (Quantifier_solver.solve qs ~max_rounds:5);
  [%expect
    {|
    ((ground (Eq (Var %guard.90) (Var %guard.89)))
     (axioms
      (((guard ((Eq (Var %guard.90) (Var %guard.89)))) (bound (x.bound.87))
        (triggers (((App h ((Var x.bound.87))))))
        (body
         (And
          ((Not (Eq (App %skolem.88 ((Var x.bound.87))) (Var x.bound.87)))
           (Eq (App h ((App %skolem.88 ((Var x.bound.87)))))
            (App h ((App %skolem.88 ((Var x.bound.87))))))))))))
     (instances
      ((And
        ((Not (Eq (App %skolem.88 ((Var a))) (Var a)))
         (Eq (App h ((App %skolem.88 ((Var a)))))
          (App h ((App %skolem.88 ((Var a)))))))))))
    Unknown_but_possibly_sat
    |}]
;;

let%expect_test "nested alternation can still derive a genuine contradiction" =
  let qs = Quantifier_solver.create () in
  let x = Tvar.of_string "x" in
  let y = Tvar.of_string "y" in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let f x : Formula.any = App (Tvar.of_string "f", [ x ]) in
  let h x : Formula.any = App (Tvar.of_string "h", [ x ]) in
  ignore
    (Quantifier_solver.assert_formula
       qs
       (forall_raw
          ( [ x ]
          , [ [ h (Var x) ] ]
          , Exists
              ( [ y ]
              , Formula.widen_quantified
                  (And [ Eq (f (Var x), Var y); Not (Eq (f (Var x), Var y)) ])
              ) ))
     : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       qs
       (Formula.widen_quantified (Eq (h a, h a)))
     : _ Or_error.t);
  print_s
    [%sexp
      (Quantifier_solver.solve qs ~max_rounds:2 : Quantifier_solver.Result.t)];
  [%expect
    {|
    (Unsat
     (core
      ((Quantifier_instance
        (body
         (And
          ((Eq (App f ((Var x.bound.99))) (App %skolem.101 ((Var x.bound.99))))
           (Not
            (Eq (App f ((Var x.bound.99))) (App %skolem.101 ((Var x.bound.99))))))))
        (bound_values ((x.bound.99 (Var a))))
        (instance
         (And
          ((Eq (App f ((Var a))) (Var %skolem.102))
           (Not (Eq (App f ((Var a))) (Var %skolem.102))))))))))
    |}]
;;

let%expect_test "term ITE in an axiom body expands at the ground instance" =
  let x = Tvar.of_string "x" in
  let a : Formula.any = Var (Tvar.of_string "a") in
  let b : Formula.any = Var (Tvar.of_string "b") in
  let c : Formula.any = Var (Tvar.of_string "c") in
  let h x : Formula.any = App (Tvar.of_string "h", [ x ]) in
  let quantified =
    forall_raw ([ x ], [ [ h (Var x) ] ], Eq (Ite (Eq (Var x, a), b, c), c))
  in
  print_elaborate quantified;
  let qs = Quantifier_solver.create () in
  ignore (Quantifier_solver.assert_formula qs quantified : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       qs
       (Formula.widen_quantified (Eq (h a, h a)))
     : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       qs
       (Formula.widen_quantified (Not (Eq (b, c))))
     : _ Or_error.t);
  print_s
    [%sexp
      (Quantifier_solver.solve qs ~max_rounds:2 : Quantifier_solver.Result.t)];
  [%expect
    {|
    ((ground (Eq (Var %guard.105) (Var %guard.104)))
     (axioms
      (((guard ((Eq (Var %guard.105) (Var %guard.104)))) (bound (x.bound.103))
        (triggers (((App h ((Var x.bound.103))))))
        (body (Eq (Ite (Eq (Var x.bound.103) (Var a)) (Var b) (Var c)) (Var c)))))))
    (Unsat
     (core
      ((Theory_lemma (Or ((Eq (Var a) (Var a)))))
       (Asserted
        (Or
         ((And ((Eq (Var a) (Var a)) (Eq (Var b) (Var c))))
          (And ((Not (Eq (Var a) (Var a))) (Eq (Var c) (Var c)))))))
       (Asserted (Not (Eq (Var b) (Var c)))))))
    |}]
;;
