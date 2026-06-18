open! Core
open! Feel.Import

(* Composite theory dispatching to both Uninterpreted_functions and Tvar_types. *)
module Combined_theory = struct
  type t =
    { uf : Uninterpreted_functions.t
    ; tt : Tvar_types.t
    }

  let assert_literal t ~decision_level ~literal =
    Uninterpreted_functions.assert_literal t.uf ~decision_level ~literal;
    Tvar_types.assert_literal t.tt ~decision_level ~literal
  ;;

  let maybe_get_lemma t = exclave_
    match (Uninterpreted_functions.maybe_get_lemma t.uf [@nontail]) with
    | `Consistent -> Tvar_types.maybe_get_lemma t.tt
    | lemma -> lemma
  ;;

  let undo t ~to_decision_level_excl =
    Uninterpreted_functions.undo t.uf ~to_decision_level_excl;
    Tvar_types.undo t.tt ~to_decision_level_excl
  ;;

  let on_new_var t ~var =
    Uninterpreted_functions.on_new_var t.uf ~var;
    Tvar_types.on_new_var t.tt ~var
  ;;
end

type t =
  { solver : Feel.Solver.t
  ; uf : Uninterpreted_functions.t
  ; tt : Tvar_types.t
  ; encoding : Formula.Encoding.t
  ; mutable scopes : int list (* activation literals, innermost first *)
  }

let create () =
  let uf = Uninterpreted_functions.create ~atoms:[] in
  let tt = Tvar_types.create () in
  let combined = { Combined_theory.uf; tt } in
  let solver =
    Feel.Solver.create
      ~theory:(Feel.Theory.pack (module Combined_theory) combined)
      ()
  in
  { solver; uf; tt; encoding = Formula.Encoding.create (); scopes = [] }
;;

(* OR's [-activation] into every clause in [clauses], so each clause is
   vacuously satisfied (by [-activation]) unless [activation] is asserted. *)
let guard_clauses ~activation clauses =
  List.map clauses ~f:(fun clause -> Array.append [| -activation |] clause)
;;

let assert_formula t formula : [ `Ok | `Unsat of int array ] =
  let checkpoint = Formula.Encoding.checkpoint t.encoding in
  let clauses = Formula.encode t.encoding formula in
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
      | `Has_type (var, typ) ->
        Tvar_types.add_atom t.tt ~atom:(`Has_type (var, typ)) ~sat_var;
        (* Mutual exclusion: ¬(x is Int) ∨ ¬(x is Float). Added when the
           second of the two type atoms for a variable is first registered. *)
        let other_typ =
          match (typ : Tvar_types.Type.t) with
          | Int -> Tvar_types.Type.Float
          | Float -> Tvar_types.Type.Int
        in
        (match Tvar_types.sat_var_for t.tt var other_typ with
         | None -> ()
         | Some other_sat_var ->
           ignore
             (Feel.Solver.add_clause
                t.solver
                ~clause:[| -sat_var; -other_sat_var |]
              : [ `Ok | `Unsat of _ ])));
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

let solve ?time_bound ?(assumptions = [||]) t =
  let scope_assumptions = Array.of_list t.scopes in
  let assumptions = Array.append scope_assumptions assumptions in
  Feel.Solver.solve ?time_bound ~assumptions t.solver
;;

let stats t = Feel.Solver.stats t.solver

let assert_type t var typ =
  ignore (assert_formula t (Formula.Atom (`Has_type (var, typ))) : [ `Ok | `Unsat of _ ])
;;

let get_type t var = Tvar_types.get_type t.tt var
