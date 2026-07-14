open! Core
open! Feel.Import

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

module Combined_theory = struct
  type t =
    { egraph : Formula_egraph_uf.t
    ; tt : Tvar_types.t
    ; bb : Branch_and_bound.t
    ; encoding : Encoding.t
    ; bridge : Uf_la_bridge.t
    }

  (* Converts a lemma atom coming back from [Formula_egraph_uf] (a bare
     [Formula.any] equality) to the [Atom.t] its originating shape used, so it
     can be looked up in [Encoding]'s atom<->sat-var table. A bare [Var]/[App]
     is (per the [Type_var] tag added specifically to disambiguate this) always
     the UF role; anything else is the type role. *)
  let egraph_atom_to_atom (`Eq (a, _) as atom : Formula_egraph_uf.Atom.t)
    : Atom.t
    =
    match Formula.op a with
    | Var _ | App _ -> (atom :> Atom.t)
    | _ ->
      let (`Eq (a, b)) = atom in
      `Type_eq
        ( Or_error.ok_exn (Encoding.type_expr_of a)
        , Or_error.ok_exn (Encoding.type_expr_of b) )
  ;;

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
        ~atom:
          (`Eq
            (Encoding.type_expr_to_formula a, Encoding.type_expr_to_formula b))
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

  let maybe_get_lemma t = exclave_
    match Formula_egraph_uf.maybe_get_lemma t.egraph [@nontail] with
    | `Lemma literals ->
      lemma_to_clause literals ~sat_var_for_atom:(fun atom ->
        Encoding.sat_var_for_atom t.encoding (egraph_atom_to_atom atom))
    | `Consistent ->
      (match Tvar_types.maybe_get_lemma t.tt [@nontail] with
       | `Lemma literals ->
         lemma_to_clause
           literals
           ~sat_var_for_atom:(fun (`Type_eq (a, b) : Tvar_types.Atom.t) ->
             Encoding.sat_var_for_atom t.encoding (`Type_eq (a, b)))
       | `Consistent ->
         (match Branch_and_bound.maybe_get_lemma t.bb [@nontail] with
          | `Lemma literals ->
            lemma_to_clause
              literals
              ~sat_var_for_atom:(fun (atom : Branch_and_bound.Atom.t) ->
                Encoding.sat_var_for_atom t.encoding (atom :> Atom.t))
          | `Consistent ->
            (match
               Uf_la_bridge.maybe_get_lemma
                 t.bridge
                 ~uf_equal:(fun a b ->
                   Formula_egraph_uf.atom_value
                     t.egraph
                     ~atom:(`Eq (Formula.Var a, Formula.Var b)))
                 ~get_type:(Tvar_types.get_type t.tt)
             with
             | `Consistent -> `Consistent
             | `Lemma literals ->
               lemma_to_clause literals ~sat_var_for_atom:(fun atom ->
                 Encoding.sat_var_for_atom t.encoding atom))))
  ;;

  let undo t ~to_decision_level_excl =
    Formula_egraph_uf.undo t.egraph ~to_decision_level_excl;
    Tvar_types.undo t.tt ~to_decision_level_excl;
    Branch_and_bound.undo t.bb ~to_decision_level_excl
  ;;

  let on_new_var (_ : t) ~var:(_ : int) =
    (* Most theory atoms (and their SAT vars) are registered up front, as new
       formulas are asserted; the sole exception is [Uf_la_bridge], which
       lazily introduces La atoms on demand via [maybe_get_lemma]'s returned
       clause -- the SAT engine allocates those vars itself, so there's
       nothing to react to here either way. *)
    ()
  ;;
end

type t =
  { solver : Feel.Solver.t
  ; egraph : Formula_egraph_uf.t
  ; tt : Tvar_types.t
  ; bb : Branch_and_bound.t
  ; encoding : Encoding.t
  ; bridge : Uf_la_bridge.t
  ; mutable scopes : int list (* activation literals, innermost first *)
  ; formula_by_root_lit : Formula.any Int.Table.t
  }

let create () =
  let egraph = Formula_egraph_uf.create ~atoms:[] in
  let tt = Tvar_types.create () in
  let bb = Branch_and_bound.create () in
  let encoding = Encoding.create () in
  let bridge = Uf_la_bridge.create () in
  let combined = { Combined_theory.egraph; tt; bb; encoding; bridge } in
  let solver =
    Feel.Solver.create
      ~theory:(Feel.Theory.pack (module Combined_theory) combined)
      ()
  in
  let t =
    { solver
    ; egraph
    ; tt
    ; bb
    ; encoding
    ; bridge
    ; scopes = []
    ; formula_by_root_lit = Int.Table.create ()
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
        Formula_egraph_uf.add_atom
          t.egraph
          ~atom:
            (`Eq
              (Encoding.type_expr_to_formula a, Encoding.type_expr_to_formula b));
        Hashtbl.set
          t.formula_by_root_lit
          ~key:(-sat_var)
          ~data:(Formula.Not (Encoding.atom_to_formula atom));
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
  let checkpoint = Encoding.checkpoint t.encoding in
  let%bind.Or_error clauses = Encoding.encode t.encoding ~formula in
  let root_lit = (List.last_exn clauses).(0) in
  Hashtbl.set t.formula_by_root_lit ~key:root_lit ~data:formula;
  let clauses =
    match t.scopes with
    | [] -> clauses
    | activation :: _ -> guard_clauses ~activation clauses
  in
  (* New theory atoms must be registered before their sat vars are referenced by
     any clause, so that [assert_literal] (triggered by unit propagation during
     [add_clause]) recognizes them as theory atoms from the start. *)
  List.iter
    (Encoding.new_atoms_since t.encoding ~checkpoint)
    ~f:(fun ((atom, _sat_var) : Atom.t * int) ->
      match atom with
      | `Eq (a, b) ->
        Formula_egraph_uf.add_atom t.egraph ~atom:(`Eq (a, b));
        (match a, b with
         | Formula.Var va, Formula.Var vb -> Uf_la_bridge.register t.bridge va vb
         | _ -> ())
      | `Type_eq (a, b) ->
        Formula_egraph_uf.add_atom
          t.egraph
          ~atom:
            (`Eq
              (Encoding.type_expr_to_formula a, Encoding.type_expr_to_formula b))
      | `Le (_, _) -> ());
  Ok
    (List.fold_until
       clauses
       ~init:`Ok
       ~f:(fun (`Ok : [ `Ok ]) clause ->
         match Feel.Solver.add_clause t.solver ~clause with
         | `Ok -> Continue `Ok
         | `Unsat _ as unsat -> Stop unsat)
       ~finish:(fun (`Ok : [ `Ok ]) -> `Ok))
;;

let push t =
  let activation = Encoding.fresh_var t.encoding in
  t.scopes <- activation :: t.scopes
;;

let pop t =
  match t.scopes with
  | [] -> assert false
  | _ :: rest -> t.scopes <- rest
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

let solve ?time_bound ?(assumptions = [||]) t : Solver_result.t =
  let scope_assumptions = Array.of_list t.scopes in
  let assumptions = Array.append scope_assumptions assumptions in
  match Feel.Solver.solve ?time_bound ~assumptions t.solver with
  | Sat { assignments = _ } -> Sat { tvar_assignments = tvar_assignments t }
  | Unsat { core } -> Unsat { core = make_unsat_core t core }
;;

let stats t = Feel.Solver.stats t.solver

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
