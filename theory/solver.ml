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
    }
  in
  (* Pre-register pairwise disequalities between distinct base types. This makes
     the type-level EUF aware that e.g. [Int != Float], so that asserting
     [TypeEq('a, Int)] and [TypeEq('a, Float)] yields a conflict. *)
  let base_types = Type_expr.Base.all in
  List.iter base_types ~f:(fun b1 ->
    List.iter base_types ~f:(fun b2 ->
      if [%compare: Type_expr.Base.t] b1 b2 < 0
      then (
        let uf_atom =
          Uninterpreted_functions.Atom.normalize
            (`Eq (`Var (Type_expr.base_tvar b1), `Var (Type_expr.base_tvar b2)))
        in
        let sat_var = Formula.Encoding.fresh_var t.encoding in
        Uninterpreted_functions.add_atom t.type_uf ~atom:uf_atom ~sat_var;
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

let solve ?time_bound ?(assumptions = [||]) t =
  let scope_assumptions = Array.of_list t.scopes in
  let assumptions = Array.append scope_assumptions assumptions in
  Feel.Solver.solve ?time_bound ~assumptions t.solver
;;

let stats t = Feel.Solver.stats t.solver

let assert_type t var type_expr =
  ignore
    (assert_formula t (Formula.Atom (`Has_type (var, type_expr)))
     : [ `Ok | `Unsat of _ ])
;;

let get_type t var = Tvar_types.get_type t.tt var
