open! Core
open! Import

module Config = struct
  type t = { produce_proofs : bool } [@@deriving sexp_of]

  let default = { produce_proofs = false }
end

let lemma_to_clause literals ~sat_var_for_atom =
  let clause =
    List.map literals ~f:(fun (atom, polarity) ->
      let sat_var = sat_var_for_atom atom in
      if polarity then sat_var else -sat_var)
    |> Array.of_list
  in
  Array.sort clause ~compare:Int.compare;
  `Lemma { Modes.Global.global = clause }
;;

(* Key for the table of lemma certificates: the lemma clause's atoms, normalized
   and sorted so a clause looked up during proof assembly matches the lemma that
   produced it regardless of literal order or polarity. *)
module Atoms_key = struct
  module T = struct
    type t = Atom.t list [@@deriving compare, hash, sexp]
  end

  include T
  include functor Hashable.Make

  let of_atoms atoms =
    List.map atoms ~f:Atom.normalize |> List.sort ~compare:Atom.compare
  ;;
end

module Combined_theory = struct
  type t =
    { egraph : Formula_egraph_uf.t
    ; tt : Tvar_types.t
    ; bb : Branch_and_bound.t
    ; encoding : Encoding.t
    ; bare_var_eq : Bare_var_eq.t
    ; shared_tvars : Tvar.Hash_set.t
    ; produce_proofs : bool
    ; certificate_by_atoms : Lemma_certificate.t Atoms_key.Table.t
    }

  let assert_literal t ~decision_level ~literal =
    let var = Int.abs literal in
    let value = literal > 0 in
    match Encoding.atom_for_sat_var t.encoding var with
    | Some (`Eq (a, b)) ->
      Formula_egraph_uf.assert_atom
        t.egraph
        ~decision_level
        ~atom:(`Eq (a, b))
        ~value
    | Some (`Type_eq (a, b)) ->
      Formula_egraph_uf.assert_atom
        t.egraph
        ~decision_level
        ~atom:(`Type_eq (a, b))
        ~value;
      Tvar_types.assert_atom t.tt ~decision_level ~atom:(`Type_eq (a, b)) ~value;
      Branch_and_bound.assert_atom
        t.bb
        ~decision_level
        ~atom:(`Type_eq (a, b))
        ~value
    | Some (`Le _ as atom) ->
      Branch_and_bound.assert_atom t.bb ~decision_level ~atom ~value
    | None -> ()
  ;;

  (* Nelson-Oppen equality discovery for shared tvars: pairs a theory considers
     possibly equal (coincident in the LA model, or merged in the egraph) are
     handed to [Bare_var_eq], which bridges them with clauses that force the SAT
     solver to decide their arrangement. *)

  (* LA-model coincidence only matters if the pair's arrangement is observable
     by another theory: both endpoints are egraph terms (EUF/congruence can tell
     them apart), or their types aren't known to agree (a forced value equality
     would force a type conflict). Same-typed pairs outside the egraph -- e.g.
     the many coincident int vars of a MILP -- need no arrangement, and bridging
     them floods the search with useless splits. *)
  let arrangement_matters t a b =
    let in_egraph v =
      Formula_egraph_uf.mem_atom_term t.egraph (Formula.Var v)
    in
    (in_egraph a && in_egraph b)
    ||
    match Tvar_types.get_type t.tt a, Tvar_types.get_type t.tt b with
    | Some type_a, Some type_b ->
      not ([%compare.equal: Type_expr.t] type_a type_b)
    | None, _ | _, None -> true
  ;;

  let register_shared_candidates t =
    List.iter (Encoding.drain_newly_shared t.encoding) ~f:(fun tvar ->
      Hash_set.add t.shared_tvars tvar;
      Branch_and_bound.add_tvar_to_check_for_equality t.bb ~tvar);
    List.iter (Branch_and_bound.equality_candidates t.bb) ~f:(fun (a, b) ->
      if arrangement_matters t a b
      then Bare_var_eq.register_candidate t.bare_var_eq a b);
    Hash_set.to_list t.shared_tvars
    |> List.filter_map ~f:(fun tvar ->
      let term : Formula.any = Var tvar in
      if Formula_egraph_uf.mem_term t.egraph term
      then Some (Formula_egraph_uf.canonical_term t.egraph ~term, tvar)
      else None)
    |> List.sort_and_group ~compare:(fun (repr1, _) (repr2, _) ->
      Formula.compare_any repr1 repr2)
    |> List.iter ~f:(fun group ->
      match List.map group ~f:snd |> List.sort ~compare:Tvar.compare with
      | [] | [ _ ] -> ()
      | first :: rest ->
        List.iter rest ~f:(fun tvar ->
          Bare_var_eq.register_candidate t.bare_var_eq first tvar))
  ;;

  (* Reconstruct the [Type_theory] certificate from the lemma clause, which is
     [(Type_eq (Var v, type1), false); (Type_eq (Var v, type2), false)] with
     [type1]/[type2] structurally incompatible. *)
  let type_theory_certificate (literals : (Tvar_types.Atom.t * bool) list)
    : Lemma_certificate.t
    =
    match literals with
    | [ ((`Type_eq (_, type1) as a1), _); ((`Type_eq (_, type2) as a2), _) ] ->
      Type_theory
        { left = type1
        ; right = type2
        ; premises = [ (a1 :> Atom.t); (a2 :> Atom.t) ]
        }
    | _ -> raise_s [%message "unexpected type-theory lemma shape"]
  ;;

  let bb_certificate t : Lemma_certificate.t =
    match Branch_and_bound.last_lemma t.bb with
    | Linear_arithmetic combination ->
      Linear_arithmetic
        { combination =
            List.map combination ~f:(fun (atom, coeff) ->
              (atom :> Atom.t), coeff)
        }
    | Integer_split { variable; floor; ceil } ->
      Integer_split { variable; floor; ceil }
    | None -> raise_s [%message "branch-and-bound produced no lemma to explain"]
  ;;

  (* Reconstruct the [Bare_var_eq] certificate from the lemma clause, whose
     shape identifies which cross-theory consequence fired (see
     [check_bare_var_eq]). *)
  let bare_var_eq_certificate (literals : (Atom.t * bool) list)
    : Lemma_certificate.t
    =
    let var_of_uf = function
      | `Eq (Formula.Var a, Formula.Var b) -> Some (a, b)
      | _ -> None
    in
    match literals with
    | [ (`Type_eq (Type_expr.Var a, Type_expr.Var b), true); (uf, false) ]
      when Option.is_some (var_of_uf uf) ->
      ignore (var_of_uf uf);
      Bare_var_eq (Equality_implies_type_equality (a, b))
    | [ (`Le (le, bound), true); (uf, false) ]
      when Option.is_some (var_of_uf uf) ->
      let left, right = Option.value_exn (var_of_uf uf) in
      let forward = Linear_expr.(var left - var right) in
      let direction : Proof.Theory_certificate.Bare_var_eq.Le_direction.t =
        if [%compare.equal: Linear_expr.t * Q.t] (le, bound) (forward, Q.zero)
        then Left_le_right
        else Right_le_left
      in
      Bare_var_eq (Equality_implies_le { left; right; direction })
    | (uf, true) :: rest when Option.is_some (var_of_uf uf) ->
      ignore rest;
      let a, b = Option.value_exn (var_of_uf uf) in
      Bare_var_eq (Numeric_coincidence_implies_equality (a, b))
    | _ -> raise_s [%message "unexpected bare-var-eq lemma shape"]
  ;;

  let record_certificate t ~atoms ~certificate =
    if t.produce_proofs
    then
      Hashtbl.set
        t.certificate_by_atoms
        ~key:(Atoms_key.of_atoms atoms)
        ~data:certificate
  ;;

  let maybe_get_lemma t = exclave_
    match Formula_egraph_uf.maybe_get_lemma t.egraph [@nontail] with
    | `Lemma literals ->
      let atoms = List.map literals ~f:(fun (atom, _) -> (atom :> Atom.t)) in
      if t.produce_proofs
      then (
        match Formula_egraph_uf.last_certificate t.egraph with
        | Some euf -> record_certificate t ~atoms ~certificate:(Euf euf)
        | None -> raise_s [%message "egraph lemma without a certificate"]);
      lemma_to_clause literals ~sat_var_for_atom:(fun atom ->
        Encoding.sat_var_for_atom t.encoding (atom :> Atom.t))
    | `Consistent ->
      (match Tvar_types.maybe_get_lemma t.tt [@nontail] with
       | `Lemma literals ->
         record_certificate
           t
           ~atoms:(List.map literals ~f:(fun (atom, _) -> (atom :> Atom.t)))
           ~certificate:(type_theory_certificate literals);
         lemma_to_clause
           literals
           ~sat_var_for_atom:(fun (`Type_eq (a, b) : Tvar_types.Atom.t) ->
             Encoding.sat_var_for_atom t.encoding (`Type_eq (a, b)))
       | `Consistent ->
         (match Branch_and_bound.maybe_get_lemma t.bb [@nontail] with
          | `Lemma literals ->
            record_certificate
              t
              ~atoms:(List.map literals ~f:(fun (atom, _) -> (atom :> Atom.t)))
              ~certificate:(bb_certificate t);
            lemma_to_clause
              literals
              ~sat_var_for_atom:(fun (atom : Branch_and_bound.Atom.t) ->
                Encoding.sat_var_for_atom t.encoding (atom :> Atom.t))
          | `Consistent ->
            register_shared_candidates t;
            (match
               Bare_var_eq.maybe_get_lemma
                 t.bare_var_eq
                 ~eq_value:(fun a b ->
                   Formula_egraph_uf.atom_value
                     t.egraph
                     ~atom:(`Eq (Formula.Var a, Formula.Var b)))
                 ~theory_of:(Encoding.theory_for_tvar t.encoding)
                 ~get_type:(Tvar_types.get_type t.tt)
             with
             | `Consistent -> `Consistent
             | `Lemma literals ->
               record_certificate
                 t
                 ~atoms:(List.map literals ~f:fst)
                 ~certificate:(bare_var_eq_certificate literals);
               lemma_to_clause literals ~sat_var_for_atom:(fun atom ->
                 Encoding.sat_var_for_atom t.encoding atom))))
  ;;

  let certificate_for_atoms t atoms =
    Hashtbl.find t.certificate_by_atoms (Atoms_key.of_atoms atoms)
  ;;

  let undo t ~to_decision_level_excl =
    Formula_egraph_uf.undo t.egraph ~to_decision_level_excl;
    Tvar_types.undo t.tt ~to_decision_level_excl;
    Branch_and_bound.undo t.bb ~to_decision_level_excl
  ;;

  let on_new_var t ~var =
    match Encoding.atom_for_sat_var t.encoding var with
    | None | Some (`Le _) -> ()
    | Some (`Eq (a, b)) ->
      Formula_egraph_uf.add_atom t.egraph ~atom:(`Eq (a, b));
      (match a, b with
       | Formula.Var a, Formula.Var b -> Bare_var_eq.register t.bare_var_eq a b
       | _ -> ())
    | Some (`Type_eq (a, b)) ->
      Formula_egraph_uf.add_atom t.egraph ~atom:(`Type_eq (a, b))
  ;;
end

type t =
  { solver : Feel.Solver.t
  ; egraph : Formula_egraph_uf.t
  ; tt : Tvar_types.t
  ; bb : Branch_and_bound.t
  ; encoding : Encoding.t
  ; combined : Combined_theory.t
  ; mutable scopes : int list (* activation literals, innermost first *)
  ; (* Formulas asserted in each active scope, innermost first, for evaluating a
       returned model. Mirrors [scopes] but retains the rich formulas. *)
    mutable asserted_scopes : Formula.any list list
  ; formula_by_root_lit : Formula.any Int.Table.t
  ; proof_generation : Proof_generation.t option
  ; (* Asserted formulas not yet registered in the egraph for e-matching.
       Registration walks the whole formula tree and adds every node to the
       congruence closure, which is pure overhead for solving, so it's deferred
       until [egraph] is called. *)
    pending_egraph_terms : Formula.any Vec.Value.t
  }

let create ?(config = Config.default) () =
  let egraph = Formula_egraph_uf.create ~atoms:[] in
  let tt = Tvar_types.create () in
  let bb = Branch_and_bound.create () in
  let encoding = Encoding.create ~produce_proofs:config.produce_proofs () in
  let bare_var_eq = Bare_var_eq.create () in
  let combined =
    { Combined_theory.egraph
    ; tt
    ; bb
    ; encoding
    ; bare_var_eq
    ; shared_tvars = Tvar.Hash_set.create ()
    ; produce_proofs = config.produce_proofs
    ; certificate_by_atoms = Atoms_key.Table.create ()
    }
  in
  let solver =
    Feel.Solver.create
      ~produce_proofs:config.produce_proofs
      ~theory:(Feel.Theory.pack (module Combined_theory) combined)
      ()
  in
  let t =
    { solver
    ; egraph
    ; tt
    ; bb
    ; encoding
    ; combined
    ; scopes = []
    ; asserted_scopes = [ [] ]
    ; formula_by_root_lit = Int.Table.create ()
    ; proof_generation =
        (if config.produce_proofs
         then Some (Proof_generation.create ())
         else None)
    ; pending_egraph_terms = Vec.Value.create ()
    }
  in
  (* Pre-register pairwise disequalities between distinct base types. This makes
     the type-level EGRAPH aware that e.g. [Int ≠ Float], so that asserting
     types unifying a variable with both [Int] and [Float] yields a conflict. *)
  let base_types = Type_expr.Base.all in
  List.iter base_types ~f:(fun b1 ->
    List.iter base_types ~f:(fun b2 ->
      if [%compare: Type_expr.Base.t] b1 b2 < 0
      then (
        let (`Type_eq (a, b) : Tvar_types.Atom.t) =
          `Type_eq (Type_expr.Base b1, Type_expr.Base b2)
        in
        let atom : Atom.t = `Type_eq (a, b) in
        let sat_var = Encoding.sat_var_for_atom t.encoding atom in
        let axiom = Formula.Not (Encoding.atom_to_formula atom) in
        Hashtbl.set t.formula_by_root_lit ~key:(-sat_var) ~data:axiom;
        (* These base-type disequalities are axioms, not user assertions, but a
           conflict may cite them; register them as proof premises so proof
           production can name them. *)
        Option.iter t.proof_generation ~f:(fun proof_generation ->
          Proof_generation.assert_formula proof_generation axiom);
        ignore
          (Feel.Solver.add_clause t.solver ~clause:[| -sat_var |]
           : [ `Ok | `Unsat of _ ]))));
  t
;;

(* OR's [-activation] into every clause in [clauses], so each clause is
   vacuously satisfied (by [-activation]) unless [activation] is asserted. *)
let guard_clauses ~activation clauses =
  List.map clauses ~f:(fun clause -> Array.append [| -activation |] clause)
;;

let assert_formula t (formula : Formula.any)
  : [ `Ok | `Unsat of Feel.Sat_result.Core_clause.t list ] Or_error.t
  =
  let%bind.Or_error clauses = Encoding.encode t.encoding ~formula in
  Vec.Value.push t.pending_egraph_terms formula;
  let root_lit = (List.last_exn clauses).(0) in
  Hashtbl.set t.formula_by_root_lit ~key:root_lit ~data:formula;
  (match t.asserted_scopes with
   | current :: outer -> t.asserted_scopes <- (formula :: current) :: outer
   | [] -> assert false);
  let clauses =
    match t.scopes with
    | [] -> clauses
    | activation :: _ -> guard_clauses ~activation clauses
  in
  let result =
    List.fold_until
      clauses
      ~init:`Ok
      ~f:(fun (`Ok : [ `Ok ]) clause ->
        match Feel.Solver.add_clause t.solver ~clause with
        | `Ok -> Continue `Ok
        | `Unsat _ as unsat -> Stop unsat)
      ~finish:(fun (`Ok : [ `Ok ]) -> `Ok)
  in
  (match result, t.proof_generation with
   | `Ok, Some proof_generation ->
     Proof_generation.assert_formula proof_generation formula
   | (`Ok | `Unsat _), None | `Unsat _, Some _ -> ());
  Ok result
;;

let push t =
  let activation = Encoding.fresh_var t.encoding in
  t.scopes <- activation :: t.scopes;
  t.asserted_scopes <- [] :: t.asserted_scopes;
  Option.iter t.proof_generation ~f:Proof_generation.push
;;

let pop t =
  match t.scopes, t.asserted_scopes with
  | [], _ | _, ([] | [ _ ]) -> assert false
  | _ :: rest, _ :: asserted_rest ->
    t.scopes <- rest;
    t.asserted_scopes <- asserted_rest;
    Option.iter t.proof_generation ~f:Proof_generation.pop
;;

let clause_to_formula t (literals : int array) : Formula.any option =
  let formulas =
    Array.filter_map literals ~f:(fun lit ->
      let var = Int.abs lit in
      match Encoding.atom_for_sat_var t.encoding var with
      | Some atom ->
        let formula = Encoding.atom_to_formula atom in
        if lit > 0 then Some formula else Some (Formula.Not formula)
      | None -> None)
  in
  if Array.length formulas = Array.length literals
  then Some (Formula.Or (Array.to_list formulas))
  else None
;;

let make_unsat_core t (core_clauses : Feel.Sat_result.Core_clause.t list)
  : Solver_result.Core_step.t list
  =
  List.filter_map core_clauses ~f:(fun { literals; is_theory } ->
    if is_theory
    then
      clause_to_formula t literals
      |> Option.map ~f:(fun f -> Solver_result.Core_step.Theory_lemma f)
    else
      (* Scan every literal for a formula_by_root_lit match. This handles both
         plain unit clauses and push-scope guarded clauses of the form
         [-activation; root_lit], where root_lit is the Tseitin root for a
         complex formula. *)
      Array.find_map literals ~f:(fun lit ->
        Hashtbl.find t.formula_by_root_lit lit
        |> Option.map ~f:(fun f -> Solver_result.Core_step.Asserted f)))
;;

let egraph_repr t ~(egraph_terms : Formula.Any.Set.t) tvar : Formula.any option =
  let term : Formula.any = Var tvar in
  if not (Set.mem egraph_terms term)
  then None
  else (
    match Formula_egraph_uf.canonical_term t.egraph ~term with
    | Var v when Tvar.equal v tvar -> None
    | repr -> Some repr)
;;

let tvar_assignments t : Tvar_assignment.t Tvar.Map.t =
  let egraph_terms =
    Formula_egraph_uf.registered_terms t.egraph |> Formula.Any.Set.of_list
  in
  let egraph_tvars =
    Set.to_list egraph_terms
    |> List.filter_map ~f:(function
      | Formula.Var tvar -> Some tvar
      | _ -> None)
  in
  let all_tvars =
    List.concat
      [ Tvar_types.all_typed_vars t.tt
      ; Branch_and_bound.all_numeric_vars t.bb
      ; egraph_tvars
      ]
    |> List.dedup_and_sort ~compare:[%compare: Tvar.t]
  in
  List.map all_tvars ~f:(fun tvar ->
    ( tvar
    , { Tvar_assignment.type_ = Tvar_types.get_type t.tt tvar
      ; numeric = Branch_and_bound.assignment t.bb ~tvar
      ; euf_repr = egraph_repr t ~egraph_terms tvar
      } ))
  |> Tvar.Map.of_alist_exn
;;

(* The boolean value the solver assigned to each registered theory atom,
   resolving atoms via the encoding's atom<->sat-var table and skipping atoms
   the solver left unassigned. *)
let atom_values t ~(assignments : bool option array) : bool Atom.Map.t =
  Encoding.atoms t.encoding
  |> List.filter_map ~f:(fun (atom, sat_var) ->
    if sat_var >= 0 && sat_var < Array.length assignments
    then
      Option.map assignments.(sat_var) ~f:(fun value ->
        Atom.normalize atom, value)
    else None)
  |> Atom.Map.of_alist_reduce ~f:(fun _ v -> v)
;;

let solve ?time_bound ?(assumptions = [||]) t : Solver_result.t =
  let scope_assumptions = Array.of_list t.scopes in
  let assumptions = Array.append scope_assumptions assumptions in
  match Feel.Solver.solve ?time_bound ~assumptions t.solver with
  | Sat { assignments } ->
    let model =
      { Model.atom_values = atom_values t ~assignments
      ; tvar_assignments = tvar_assignments t
      ; euf_classes =
          Formula_egraph_uf.classes t.egraph |> Formula.Any.Map.of_alist_exn
      }
    in
    Sat { model }
  | Unsat { core } ->
    let proof =
      Option.bind t.proof_generation ~f:(fun proof_generation ->
        let refutation_clauses =
          Option.value_exn (Feel.Solver.last_refutation t.solver)
        in
        Proof_generation.unsat_proof
          proof_generation
          ~encoding:t.encoding
          ~certificate_for_atoms:
            (Combined_theory.certificate_for_atoms t.combined)
          ~formula_by_root_lit:t.formula_by_root_lit
          ~scope_vars:t.scopes
          ~refutation_clauses)
    in
    Unsat { core = make_unsat_core t core; proof }
;;

let stats t = Feel.Solver.stats t.solver

(* Formulas asserted in the currently active scopes, which a returned model must
   satisfy. Includes the base-type disequality axioms registered at [create]. *)
let active_asserted_formulas t =
  let asserted = List.concat_map t.asserted_scopes ~f:Fn.id in
  let axioms =
    Hashtbl.data t.formula_by_root_lit
    |> List.filter ~f:(function
      | Formula.Not (Eq ((Bool | Int | Float), (Bool | Int | Float))) -> true
      | _ -> false)
  in
  axioms @ asserted
;;

let check_model t (model : Model.t) =
  Model.check model ~asserted_formulas:(active_asserted_formulas t)
;;

let egraph t =
  Vec.Value.to_list t.pending_egraph_terms
  |> List.iter ~f:(fun term -> Formula_egraph_uf.add_term t.egraph ~term);
  Vec.Value.clear t.pending_egraph_terms;
  t.egraph
;;

let assert_type t var type_expr =
  ignore
    (Or_error.ok_exn
       (assert_formula
          t
          (Encoding.atom_to_formula
             (Tvar_types.has_type var type_expr :> Atom.t)))
     : [ `Ok | `Unsat of _ ])
;;

let get_type t var = Tvar_types.get_type t.tt var
