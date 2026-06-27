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
    ; encoding : Formula.Encoding.t
    }

  let assert_literal t ~decision_level ~literal =
    let var = Int.abs literal in
    let value = literal > 0 in
    match Formula.Encoding.atom_for_sat_var t.encoding var with
    | Some (#Uf_term.Uf.Atom.t as atom) ->
      Uf_term.Uf.assert_atom t.uf ~decision_level ~atom ~value
    | Some (`Type_eq (a, b)) ->
      Type_expr.Uf.assert_atom t.tuf ~decision_level ~atom:(`Eq (a, b)) ~value;
      Tvar_types.assert_atom t.tt ~decision_level ~atom:(`Type_eq (a, b)) ~value
    | Some (`Le _) | None -> ()
  ;;

  let maybe_get_lemma t = exclave_
    match Uf_term.Uf.maybe_get_lemma t.uf [@nontail] with
    | `Lemma literals ->
      lemma_to_clause literals ~sat_var_for_atom:(fun atom ->
        Formula.Encoding.sat_var_for_atom
          t.encoding
          (atom : Uf_term.Uf.Atom.t :> Atom.t))
    | `Consistent ->
      (match Type_expr.Uf.maybe_get_lemma t.tuf [@nontail] with
       | `Lemma literals ->
         lemma_to_clause
           literals
           ~sat_var_for_atom:(fun (`Eq (a, b) : Type_expr.Uf.Atom.t) ->
             Formula.Encoding.sat_var_for_atom t.encoding (`Type_eq (a, b)))
       | `Consistent ->
         (match Tvar_types.maybe_get_lemma t.tt [@nontail] with
          | `Consistent -> `Consistent
          | `Lemma literals ->
            lemma_to_clause
              literals
              ~sat_var_for_atom:(fun (`Type_eq (a, b) : Tvar_types.Atom.t) ->
                Formula.Encoding.sat_var_for_atom t.encoding (`Type_eq (a, b)))))
  ;;

  let undo t ~to_decision_level_excl =
    Uf_term.Uf.undo t.uf ~to_decision_level_excl;
    Type_expr.Uf.undo t.tuf ~to_decision_level_excl;
    Tvar_types.undo t.tt ~to_decision_level_excl
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
  ; encoding : Formula.Encoding.t
  ; mutable scopes : int list (* activation literals, innermost first *)
  ; formula_by_root_lit : Formula.t Int.Table.t
  }

let create () =
  let uf = Uf_term.Uf.create ~atoms:[] in
  let tuf = Type_expr.Uf.create ~atoms:[] in
  let tt = Tvar_types.create () in
  let encoding = Formula.Encoding.create () in
  let combined = { Combined_theory.uf; tuf; tt; encoding } in
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
        let sat_var = Formula.Encoding.sat_var_for_atom t.encoding atom in
        Type_expr.Uf.add_atom t.tuf ~atom:tuf_atom;
        Hashtbl.set
          t.formula_by_root_lit
          ~key:(-sat_var)
          ~data:(Formula.Not (Formula.Atom atom));
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

let assert_formula t formula
  : [ `Ok | `Unsat of Feel.Sat_result.Core_clause.t list ]
  =
  let checkpoint = Formula.Encoding.checkpoint t.encoding in
  let clauses = Formula.encode t.encoding formula in
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
    (Formula.Encoding.new_atoms_since t.encoding ~checkpoint)
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
  let activation = Formula.Encoding.fresh_var t.encoding in
  t.scopes <- activation :: t.scopes
;;

let pop t =
  match t.scopes with
  | [] -> assert false
  | _ :: rest -> t.scopes <- rest
;;

let clause_to_formula t (literals : int array) : Formula.t option =
  let formulas =
    Array.filter_map literals ~f:(fun lit ->
      let var = Int.abs lit in
      match Formula.Encoding.atom_for_sat_var t.encoding var with
      | Some atom ->
        if lit > 0
        then Some (Formula.Atom atom)
        else Some (Formula.Not (Formula.Atom atom))
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

let solve ?time_bound ?(assumptions = [||]) t : Solver_result.t =
  let scope_assumptions = Array.of_list t.scopes in
  let assumptions = Array.append scope_assumptions assumptions in
  match Feel.Solver.solve ?time_bound ~assumptions t.solver with
  | Sat { assignments } -> Sat { assignments }
  | Unsat { core } -> Unsat { core = make_unsat_core t core }
;;

let stats t = Feel.Solver.stats t.solver

let assert_type t var type_expr =
  ignore
    (assert_formula
       t
       (Formula.Atom (Tvar_types.has_type var type_expr :> Atom.t))
     : [ `Ok | `Unsat of _ ])
;;

let get_type t var = Tvar_types.get_type t.tt var
