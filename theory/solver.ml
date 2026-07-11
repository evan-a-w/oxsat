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
    { uf : Uf_term.Uf.t
    ; tuf : Type_expr.Uf.t
    ; tt : Tvar_types.t
    ; bb : Branch_and_bound.t
    ; encoding : Encoding.t
    }

  let assert_literal t ~decision_level ~literal =
    let var = Int.abs literal in
    let value = literal > 0 in
    match Encoding.atom_for_sat_var t.encoding var with
    | Some (#Uf_term.Uf.Atom.t as atom) ->
      Uf_term.Uf.assert_atom t.uf ~decision_level ~atom ~value
    | Some (`Type_eq (a, b)) ->
      Type_expr.Uf.assert_atom t.tuf ~decision_level ~atom:(`Eq (a, b)) ~value;
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
    match Uf_term.Uf.maybe_get_lemma t.uf [@nontail] with
    | `Lemma literals ->
      lemma_to_clause literals ~sat_var_for_atom:(fun atom ->
        Encoding.sat_var_for_atom
          t.encoding
          (atom : Uf_term.Uf.Atom.t :> Atom.t))
    | `Consistent ->
      (match Type_expr.Uf.maybe_get_lemma t.tuf [@nontail] with
       | `Lemma literals ->
         lemma_to_clause
           literals
           ~sat_var_for_atom:(fun (`Eq (a, b) : Type_expr.Uf.Atom.t) ->
             Encoding.sat_var_for_atom t.encoding (`Type_eq (a, b)))
       | `Consistent ->
         (match Tvar_types.maybe_get_lemma t.tt [@nontail] with
          | `Lemma literals ->
            lemma_to_clause
              literals
              ~sat_var_for_atom:(fun (`Type_eq (a, b) : Tvar_types.Atom.t) ->
                Encoding.sat_var_for_atom t.encoding (`Type_eq (a, b)))
          | `Consistent ->
            (match Branch_and_bound.maybe_get_lemma t.bb [@nontail] with
             | `Consistent -> `Consistent
             | `Lemma literals ->
               lemma_to_clause
                 literals
                 ~sat_var_for_atom:(fun (atom : Branch_and_bound.Atom.t) ->
                   Encoding.sat_var_for_atom t.encoding (atom :> Atom.t)))))
  ;;

  let undo t ~to_decision_level_excl =
    Uf_term.Uf.undo t.uf ~to_decision_level_excl;
    Type_expr.Uf.undo t.tuf ~to_decision_level_excl;
    Tvar_types.undo t.tt ~to_decision_level_excl;
    Branch_and_bound.undo t.bb ~to_decision_level_excl
  ;;

  let on_new_var (_ : t) ~var:(_ : int) =
    (* All theory atoms (and their SAT vars) are registered up front, as new
       formulas are asserted; no theory introduces variables lazily. *)
    ()
  ;;
end

type t =
  { solver : Feel.Solver.t
  ; uf : Uf_term.Uf.t
  ; tuf : Type_expr.Uf.t
  ; tt : Tvar_types.t
  ; bb : Branch_and_bound.t
  ; encoding : Encoding.t
  ; mutable scopes : int list (* activation literals, innermost first *)
  ; formula_by_root_lit : Formula.any Int.Table.t
  }

let create () =
  let uf = Uf_term.Uf.create ~atoms:[] in
  let tuf = Type_expr.Uf.create ~atoms:[] in
  let tt = Tvar_types.create () in
  let bb = Branch_and_bound.create () in
  let encoding = Encoding.create () in
  let combined = { Combined_theory.uf; tuf; tt; bb; encoding } in
  let solver =
    Feel.Solver.create
      ~theory:(Feel.Theory.pack (module Combined_theory) combined)
      ()
  in
  let t =
    { solver
    ; uf
    ; tuf
    ; tt
    ; bb
    ; encoding
    ; scopes = []
    ; formula_by_root_lit = Int.Table.create ()
    }
  in
  (* Pre-register pairwise disequalities between distinct base types. This makes
     the type-level EUF aware that e.g. [Int ≠ Float], so that asserting types
     unifying a variable with both [Int] and [Float] yields a conflict. *)
  let base_types = Type_expr.Base.all in
  List.iter base_types ~f:(fun b1 ->
    List.iter base_types ~f:(fun b2 ->
      if [%compare: Type_expr.Base.t] b1 b2 < 0
      then (
        let (`Eq (a, b) as tuf_atom) =
          Type_expr.Uf.Atom.normalize
            (`Eq (Type_expr.Base b1, Type_expr.Base b2))
        in
        let atom : Atom.t = `Type_eq (a, b) in
        let sat_var = Encoding.sat_var_for_atom t.encoding atom in
        Type_expr.Uf.add_atom t.tuf ~atom:tuf_atom;
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
  : [ `Ok | `Unsat of Feel.Sat_result.Core_clause.t list ]
  =
  let checkpoint = Encoding.checkpoint t.encoding in
  let clauses = Encoding.encode t.encoding ~formula in
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
      | #Uf_term.Uf.Atom.t as atom -> Uf_term.Uf.add_atom t.uf ~atom
      | `Type_eq (a, b) -> Type_expr.Uf.add_atom t.tuf ~atom:(`Eq (a, b))
      | `Le (_, _) -> ());
  List.fold_until
    clauses
    ~init:`Ok
    ~f:(fun (`Ok : [ `Ok ]) clause ->
      match Feel.Solver.add_clause t.solver ~clause with
      | `Ok -> Continue `Ok
      | `Unsat _ as unsat -> Stop unsat)
    ~finish:(fun (`Ok : [ `Ok ]) -> `Ok)
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

let euf_repr t ~(euf_terms : Uf_term.Set.t) tvar : Uf_term.t option =
  let term : Uf_term.t = `Var tvar in
  if not (Set.mem euf_terms term)
  then None
  else (
    match Uf_term.Uf.canonical_term t.uf ~term with
    | `Var v when Tvar.equal v tvar -> None
    | repr -> Some repr)
;;

let tvar_assignments t : Tvar_assignment.t Tvar.Map.t =
  let euf_terms = Uf_term.Uf.registered_terms t.uf |> Uf_term.Set.of_list in
  let euf_tvars =
    Set.to_list euf_terms
    |> List.filter_map ~f:(function
      | `Var tvar -> Some tvar
      | `App _ -> None)
  in
  let all_tvars =
    List.concat
      [ Tvar_types.all_typed_vars t.tt
      ; Branch_and_bound.all_numeric_vars t.bb
      ; euf_tvars
      ]
    |> List.dedup_and_sort ~compare:[%compare: Tvar.t]
  in
  List.map all_tvars ~f:(fun tvar ->
    ( tvar
    , { Tvar_assignment.type_ = Tvar_types.get_type t.tt tvar
      ; numeric = Branch_and_bound.assignment t.bb ~tvar
      ; euf_repr = euf_repr t ~euf_terms tvar
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
    (assert_formula
       t
       (Encoding.atom_to_formula (Tvar_types.has_type var type_expr :> Atom.t))
     : [ `Ok | `Unsat of _ ])
;;

let get_type t var = Tvar_types.get_type t.tt var
