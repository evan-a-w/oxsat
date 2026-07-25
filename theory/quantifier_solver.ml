open! Core
open! Import

module Axiom_state = struct
  type t =
    { axiom : Quantifier_axiom.Axiom.t
    ; (* The top-level [∀] this axiom came from, for a proof to cite when
         justifying its instances; [None] for a universal nested inside boolean
         structure (guarded, [proof = None] fallback). *)
      given : Formula.quantified option
    ; (* Ground instances already asserted for this axiom, so re-matching an
         already-instantiated substitution is a no-op instead of growing the
         clause set forever. *)
      seen : Formula.Any.Hash_set.t
    }
end

module Instance_provenance = struct
  type t =
    { body : Formula.any
    ; bound_values : (Tvar.t * Formula.any) list
    ; instance : Formula.any
    }
end

type t =
  { solver : Solver.t
  ; mutable axiom_states : Axiom_state.t list
  ; (* Maps each guarded-instance formula [Or [Not guard; instance]] we assert
       back to what produced it, so an [Unsat] core can cite the instantiation
       instead of showing an opaque asserted disjunction. *)
    provenance_by_guarded : Instance_provenance.t Formula.Any.Table.t
  }

let create ?config () =
  { solver = Solver.create ?config ()
  ; axiom_states = []
  ; provenance_by_guarded = Formula.Any.Table.create ()
  }
;;

let add_axiom_state t ~axiom ~given =
  t.axiom_states
  <- { Axiom_state.axiom; given; seen = Formula.Any.Hash_set.create () }
     :: t.axiom_states
;;

(* A bare top-level [∀]: register a guard-free axiom (no ground constraint until
   instantiated) and hand the [∀] itself to the proof layer to cite. *)
let register_toplevel_forall t ~bound ~triggers ~body =
  let axiom, given =
    Quantifier_elaboration.register_toplevel_forall ~bound ~triggers ~body
  in
  add_axiom_state t ~axiom ~given:(Some given);
  Solver.proof_add_quantified_given t.solver given
;;

(* A bare top-level [∃]: Skolemize to a ground body (a real constraint) and
   record both the [∃] and its witnessing substitution for the proof. *)
let assert_toplevel_exists t ~bound ~body =
  let existential : Formula.quantified = Exists (bound, body) in
  let skolems, skolem_body =
    Quantifier_elaboration.skolemize_existential ~bound body
  in
  Solver.proof_add_quantified_given t.solver existential;
  Solver.proof_note_exists_skolemization
    t.solver
    ~skolem_body
    ~existential
    ~skolems;
  Solver.assert_formula t.solver skolem_body
;;

(* A quantifier nested inside boolean structure (under [Or]/[Not], etc.): keep
   the guard encoding. Guards are synthetic, so a refutation depending on one
   yields [proof = None]. *)
let assert_nested_guarded t (formula : Formula.quantified) =
  let ground, new_axioms = Quantifier_elaboration.elaborate formula in
  List.iter new_axioms ~f:(fun axiom ->
    add_axiom_state t ~axiom ~given:None;
    Option.iter axiom.guard ~f:(Solver.proof_note_synthetic t.solver));
  Solver.assert_formula t.solver ground
;;

(* A top-level conjunction (possibly a mix of [∀], [∃], and ground facts, and
   what [And [∀..; ∃..]] elaborates to) is asserted conjunct by conjunct, so
   each quantifier is handled at top level and gets a real proof; a formula with
   no quantifier at all goes straight to the ground solver unchanged. *)
let rec assert_formula t (formula : Formula.quantified)
  : [ `Ok | `Unsat of Feel.Sat_result.Core_clause.t list ] Or_error.t
  =
  match Formula.to_any formula with
  | Some ground -> Solver.assert_formula t.solver ground
  | None ->
    (match formula with
     | And conjuncts ->
       List.fold_until
         conjuncts
         ~init:(`Ok : [ `Ok ])
         ~f:(fun `Ok conjunct ->
           match assert_formula t conjunct with
           | Ok `Ok -> Continue `Ok
           | (Ok (`Unsat _) | Error _) as stop -> Stop stop)
         ~finish:(fun `Ok -> Ok `Ok)
     | Forall (bound, triggers, body) ->
       register_toplevel_forall t ~bound ~triggers ~body;
       Ok `Ok
     | Exists (bound, body) -> assert_toplevel_exists t ~bound ~body
     | _ -> assert_nested_guarded t formula)
;;

let push t = Solver.push t.solver
let pop t = Solver.pop t.solver
let egraph t = Solver.egraph t.solver

(* Matches a whole trigger group (all terms must hold under one substitution)
   against [graph] by matching each term in turn, substituting previously bound
   variables into later terms before searching -- a plain backtracking join on
   shared bound variables. *)
let matches_for_trigger_group
  (uf : Formula_egraph_uf.t)
  ~(bound : Tvar.t list)
  (terms : Formula.any list)
  (graph : Formula_egraph.Graph.t)
  : Formula.any Tvar.Map.t list
  =
  let rec go terms (partial : Formula.any Tvar.Map.t) =
    match terms with
    | [] -> [ partial ]
    | term :: rest ->
      let remaining_bound =
        List.filter bound ~f:(fun v -> not (Map.mem partial v))
      in
      let substituted = Formula.substitute partial term in
      let query =
        Quantifier_axiom.query_of_term ~bound:remaining_bound substituted
      in
      Formula_egraph.Pattern.Query.search query ~graph
      |> List.concat_map ~f:(fun m ->
        match
          Quantifier_axiom.substitution_of_match uf ~bound:remaining_bound m
        with
        | None -> []
        | Some bindings ->
          let merged =
            Map.merge_skewed partial bindings ~combine:(fun ~key:_ _ new_ ->
              new_)
          in
          go rest merged)
  in
  go terms Tvar.Map.empty
  (* A trigger that doesn't mention every bound var leaves some of them
     unsubstituted; drop those matches rather than instantiate a non-ground
     "instance". *)
  |> List.filter ~f:(fun subst -> List.for_all bound ~f:(Map.mem subst))
;;

let instantiate t : Formula.any list =
  let uf = Solver.egraph t.solver in
  let graph = Formula_egraph_uf.egraph uf in
  List.concat_map t.axiom_states ~f:(fun { Axiom_state.axiom; given; seen } ->
    List.concat_map axiom.triggers ~f:(fun trigger ->
      matches_for_trigger_group uf ~bound:axiom.bound trigger graph)
    |> List.filter_map ~f:(fun subst ->
      let instance = Formula.substitute subst axiom.body in
      if Hash_set.mem seen instance
      then None
      else (
        Hash_set.add seen instance;
        let bound_values = Map.to_alist subst in
        (* The formula actually asserted, and the provenance key that
           [relabel_core_step] looks it up by. A top-level universal is
           guard-free: assert the bare instance and derive it in the proof by
           universal instantiation of the cited [∀]. A nested one stays guarded
           ([¬guard ∨ instance]) -- sound regardless of whether the guard is
           forced, and its synthetic guard makes any proof decline. *)
        let asserted =
          match axiom.guard with
          | None ->
            Option.iter given ~f:(fun forall ->
              Solver.proof_note_forall_instance
                t.solver
                ~instance
                ~forall
                ~bound_values);
            instance
          | Some guard -> Formula.Or [ Not guard; instance ]
        in
        Hashtbl.set
          t.provenance_by_guarded
          ~key:asserted
          ~data:{ Instance_provenance.body = axiom.body; bound_values; instance };
        Some asserted)))
;;

(* [Solver.assert_formula] can only fail on an ill-formed formula; every
   instance here is built by substituting into an already-elaborated axiom body,
   so it's always well-formed. *)
let assert_instance t (instance : Formula.any)
  : [ `Ok | `Unsat of Feel.Sat_result.Core_clause.t list ]
  =
  Or_error.ok_exn (Solver.assert_formula t.solver instance)
;;

(* Relabels a core step that is one of our guarded instances as a
   [Quantifier_instance], so the core cites the originating axiom body and
   substitution instead of the raw [Or [Not guard; instance]] disjunction. *)
let relabel_core_step t (step : Solver_result.Core_step.t)
  : Solver_result.Core_step.t
  =
  match step with
  | Asserted formula ->
    (match Hashtbl.find t.provenance_by_guarded formula with
     | Some { body; bound_values; instance } ->
       Quantifier_instance { body; bound_values; instance }
     | None -> step)
  | Theory_lemma _ | Quantifier_instance _ -> step
;;

module Result = struct
  type t =
    | Unsat of
        { core : Solver_result.Core_step.t list
        ; proof : Proof.t option [@sexp.option]
        }
    | Sat of { model : Model.t }
    | Unknown_but_possibly_sat of { model : Model.t }
  [@@deriving sexp_of]
end

(* Maps the underlying [Solver_result.t] to a [Result.t], relabeling
   instance-derived core steps and demoting a [Sat] to
   [Unknown_but_possibly_sat] whenever any universal axiom is in play: with
   axioms present, trigger-based instantiation can't certify the model against
   the universals, so the ground [Sat] is only "possibly sat". A ground problem
   (no axioms ever registered) keeps its authoritative [Sat]. *)
let classify t (result : Solver_result.t) : Result.t =
  match result with
  | Unsat { core; proof } ->
    Unsat { core = List.map core ~f:(relabel_core_step t); proof }
  | Sat { model } ->
    if List.is_empty t.axiom_states
    then Sat { model }
    else Unknown_but_possibly_sat { model }
;;

let rec solve_loop ?time_bound ?assumptions ~max_rounds ~round t
  : Solver_result.t
  =
  match Solver.solve ?time_bound ?assumptions t.solver with
  | Unsat _ as result -> result
  | Sat _ as result when round >= max_rounds -> result
  | Sat _ as result ->
    (match instantiate t with
     | [] -> result
     | new_instances ->
       (* Per [Solver.assert_formula]'s doc, a clause that conflicts immediately
          is never enforced, so a later [Solver.solve] call won't rediscover it
          -- surface it as the final result right here instead of looping past
          it. *)
       (match
          List.find_map new_instances ~f:(fun instance ->
            match assert_instance t instance with
            | `Ok -> None
            | `Unsat core -> Some core)
        with
        | Some core ->
          Unsat
            { core = Solver.unsat_core_of_core_clauses t.solver core
            ; proof = None
            }
        | None ->
          solve_loop ?time_bound ?assumptions ~max_rounds ~round:(round + 1) t))
;;

let solve ?time_bound ?assumptions ?(max_rounds = 50) t : Result.t =
  classify t (solve_loop ?time_bound ?assumptions ~max_rounds ~round:0 t)
;;
