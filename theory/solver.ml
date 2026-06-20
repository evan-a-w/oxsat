open! Core
open! Feel.Import

(* Composite theory dispatching to value-level EUF, type-level EUF, and
   Tvar_types. *)
module Combined_theory = struct
  type t =
    { uf : Uninterpreted_functions.t
    ; type_uf : Uninterpreted_functions.t
    ; tt : Tvar_types.t
    }

  let assert_literal t ~decision_level ~literal =
    Uninterpreted_functions.assert_literal t.uf ~decision_level ~literal;
    Uninterpreted_functions.assert_literal t.type_uf ~decision_level ~literal;
    Tvar_types.assert_literal t.tt ~decision_level ~literal
  ;;

  let maybe_get_lemma t = exclave_
    match Uninterpreted_functions.maybe_get_lemma t.uf [@nontail] with
    | `Consistent ->
      (match Uninterpreted_functions.maybe_get_lemma t.type_uf [@nontail] with
       | `Consistent -> Tvar_types.maybe_get_lemma t.tt
       | lemma -> lemma)
    | lemma -> lemma
  ;;

  let undo t ~to_decision_level_excl =
    Uninterpreted_functions.undo t.uf ~to_decision_level_excl;
    Uninterpreted_functions.undo t.type_uf ~to_decision_level_excl;
    Tvar_types.undo t.tt ~to_decision_level_excl
  ;;

  let on_new_var t ~var =
    Uninterpreted_functions.on_new_var t.uf ~var;
    Uninterpreted_functions.on_new_var t.type_uf ~var;
    Tvar_types.on_new_var t.tt ~var
  ;;
end

type t =
  { solver : Feel.Solver.t
  ; uf : Uninterpreted_functions.t
  ; type_uf : Uninterpreted_functions.t
  ; tt : Tvar_types.t
  ; encoding : Formula.Encoding.t
  ; mutable scopes : int list (* activation literals, innermost first *)
  ; formula_by_root_lit : Formula.t Int.Table.t
  }

let create () =
  let uf = Uninterpreted_functions.create ~atoms:[] in
  let type_uf = Uninterpreted_functions.create ~atoms:[] in
  let tt = Tvar_types.create () in
  let combined = { Combined_theory.uf; type_uf; tt } in
  let solver =
    Feel.Solver.create
      ~theory:(Feel.Theory.pack (module Combined_theory) combined)
      ()
  in
  let t =
    { solver
    ; uf
    ; type_uf
    ; tt
    ; encoding = Formula.Encoding.create ()
    ; scopes = []
    ; formula_by_root_lit = Int.Table.create ()
    }
  in
  (* Pre-register pairwise disequalities between distinct base types. This makes
     the type-level EUF aware that e.g. [Int ≠ Float], so that asserting
     [TypeEq('a, Int)] and [TypeEq('a, Float)] yields a conflict. Registered via
     [sat_var_for_atom] so [reason_formula] resolves these vars to
     [Not (Atom (Type_eq ...))] rather than silently dropping them. *)
  let base_types = Type_expr.Base.all in
  List.iter base_types ~f:(fun b1 ->
    List.iter base_types ~f:(fun b2 ->
      if [%compare: Type_expr.Base.t] b1 b2 < 0
      then (
        let sat_var =
          Formula.Encoding.sat_var_for_atom
            t.encoding
            (`Type_eq (Base b1, Base b2))
        in
        let uf_atom =
          Uninterpreted_functions.Atom.normalize
            (`Eq (`Var (Type_expr.base_tvar b1), `Var (Type_expr.base_tvar b2)))
        in
        Uninterpreted_functions.add_atom t.type_uf ~atom:uf_atom ~sat_var;
        Hashtbl.set
          t.formula_by_root_lit
          ~key:(-sat_var)
          ~data:(Formula.Not (Formula.Atom (`Type_eq (Base b1, Base b2))));
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

(* Converts a [Type_expr.t] to an [Uninterpreted_functions.Term.t] so it can be
   registered with the type-level EUF instance. Base types become constant term
   variables using the canonical tvars from [Type_expr.base_tvar]. *)
let rec type_expr_to_term : Type_expr.t -> Uninterpreted_functions.Term.t
  = function
  | Base b -> `Var (Type_expr.base_tvar b)
  | Var v -> `Var v
  | App (ctor, args) ->
    `App (~function_:ctor, ~args:(List.map args ~f:type_expr_to_term))
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
    ~f:(fun (atom, sat_var) ->
      match atom with
      | #Uninterpreted_functions.Atom.t as atom ->
        Uninterpreted_functions.add_atom t.uf ~atom ~sat_var
      | `Le (_, _) -> ()
      | `Has_type (var, type_expr) ->
        Tvar_types.add_atom t.tt ~atom:(`Has_type (var, type_expr)) ~sat_var
      | `Type_eq (te1, te2) ->
        let t1 = type_expr_to_term te1 in
        let t2 = type_expr_to_term te2 in
        let uf_atom = Uninterpreted_functions.Atom.normalize (`Eq (t1, t2)) in
        Uninterpreted_functions.add_atom t.type_uf ~atom:uf_atom ~sat_var);
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
    (assert_formula t (Formula.Atom (`Has_type (var, type_expr)))
     : [ `Ok | `Unsat of _ ])
;;

let get_type t var = Tvar_types.get_type t.tt var
