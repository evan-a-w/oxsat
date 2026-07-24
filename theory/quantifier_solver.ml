open! Core
open! Import

module Axiom_state = struct
  type t =
    { axiom : Quantifier_axiom.Axiom.t
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

let assert_formula t (formula : Formula.quantified) =
  let ground, new_axioms = Quantifier_elaboration.elaborate formula in
  List.iter new_axioms ~f:(fun axiom ->
    t.axiom_states
    <- { Axiom_state.axiom; seen = Formula.Any.Hash_set.create () }
       :: t.axiom_states);
  Solver.assert_formula t.solver ground
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
  List.concat_map t.axiom_states ~f:(fun { Axiom_state.axiom; seen } ->
    List.concat_map axiom.triggers ~f:(fun trigger ->
      matches_for_trigger_group uf ~bound:axiom.bound trigger graph)
    |> List.filter_map ~f:(fun subst ->
      let instance = Formula.substitute subst axiom.body in
      if Hash_set.mem seen instance
      then None
      else (
        Hash_set.add seen instance;
        (* Sound whether or not [axiom.guard] happens to be forced true: for a
           top-level axiom it already is (unit-asserted at elaboration time), so
           this simplifies to [instance] under unit propagation; for a nested
           axiom it's exactly the guarded consequence. In the proof, this makes
           the instance a plain propositional resolution of the guard and this
           clause -- no dedicated instantiation rule needed. *)
        let guarded = Formula.Or [ Not axiom.guard; instance ] in
        Hashtbl.set
          t.provenance_by_guarded
          ~key:guarded
          ~data:
            { Instance_provenance.body = axiom.body
            ; bound_values = Map.to_alist subst
            ; instance
            };
        Some guarded)))
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

let relabel_result t (result : Solver_result.t) : Solver_result.t =
  match result with
  | Sat _ -> result
  | Unsat { core; proof } ->
    Unsat { core = List.map core ~f:(relabel_core_step t); proof }
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

let solve ?time_bound ?assumptions ?(max_rounds = 50) t =
  relabel_result t (solve_loop ?time_bound ?assumptions ~max_rounds ~round:0 t)
;;
