open! Core
open! Feel.Import
open! Theory_core
open! Theory

(* [Forall]/[Exists]'s own triggers/body are plain ground [Formula.any] (no
   further quantifiers), distinct from the ambient [Formula.quantified] used for
   the surrounding skeleton -- hence two families of helpers. *)
let xg : Formula.any = Var (Tvar.of_string "x")
let fg arg : Formula.any = App (Tvar.of_string "f", [ arg ])
let a : Formula.quantified = Var (Tvar.of_string "a")

let forall ?(triggers = [ [ fg xg ] ]) body : Formula.quantified =
  Forall ([ Tvar.of_string "x" ], triggers, body)
;;

let exists body : Formula.quantified = Exists ([ Tvar.of_string "x" ], body)

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
      (((guard (Eq (Var %guard.2) (Var %guard.1))) (bound (x.bound.0))
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
      (((guard (Eq (Var %guard.7) (Var %guard.6))) (bound (x.bound.5))
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
      (((guard (Eq (Var %guard.10) (Var %guard.9))) (bound (x.bound.8))
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
      (((guard (Eq (Var %guard.13) (Var %guard.12))) (bound (x.bound.11))
        (triggers (((App f ((Var x.bound.11))))))
        (body (Eq (App f ((Var x.bound.11))) (Var x.bound.11))))
       ((guard (Eq (Var %guard.16) (Var %guard.15))) (bound (x.bound.14))
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

let%expect_test "manual instantiate-once: single ground match" =
  let x = Tvar.of_string "x" in
  let f_sym = Tvar.of_string "f" in
  let fx : Formula.any = App (f_sym, [ Var x ]) in
  let axiom : Quantifier_axiom.Axiom.t =
    { guard = Eq (Var (Tvar.of_string "g1"), Var (Tvar.of_string "g2"))
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
    { guard = Eq (Var (Tvar.of_string "g1"), Var (Tvar.of_string "g2"))
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
  Forall ([ x ], [ [ trigger xv ] ], body xv)
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
       (Asserted (Eq (Var %guard.19) (Var %guard.18)))
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
  [%expect {| (term_count 16) |}]
;;

let%expect_test "produce_proofs: a quantifier-driven unsat yields a checked \
                 proof, with zero special-casing needed for instances"
  =
  let qs =
    Quantifier_solver.create ~config:{ Solver.Config.produce_proofs = true } ()
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
      a0: bool ≠ int
      a1: bool ≠ float
      a2: int ≠ float
      a3: %guard.28 = %guard.27
      a4: a = b
      a5: f(a) ≠ b
      a6: %guard.28 ≠ %guard.27 ∨ f(a) = a
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ float   [assumption a1]
      s2: int ≠ float   [assumption a2]
      s3: %guard.28 = %guard.27   [assumption a3]
      s4: a = b   [assumption a4]
      s5: f(a) ≠ b   [assumption a5]
      s6: %guard.28 ≠ %guard.27 ∨ f(a) = a   [assumption a6]
      s7: false   [refutation of [s0, s1, s2, s3, s4, s5, s6]]
        refutation:
          extensions:
            e0 := (¬(%guard.27 = %guard.28) ∨ a = f(a))
          steps:
            r0: %guard.27 = %guard.28   [assumption a3]
            r1: a = b   [assumption a4]
            r2: b ≠ f(a)   [assumption a5]
            r3: a = f(a) ∨ %guard.27 ≠ %guard.28 ∨ ¬(e0)   [definition of e0]
            r4: e0   [assumption a6]
            r5: a ≠ b ∨ a ≠ f(a) ∨ b = f(a)   [EUF: b = f(a) via [a = b; a = f(a)]]
            r6: ⊥   [RUP over [r0, r1, r2, r4, r5, r3]]
    Conclusion: s7
    |}]
;;
