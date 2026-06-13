open! Core
open! Feel.Import

type t =
  { solver : Feel.Solver.t
  ; theory : Uninterpreted_functions.t
  ; encoding : Formula.Encoding.t
  ; mutable scopes : int list (* activation literals, innermost first *)
  }

let create () =
  let theory = Uninterpreted_functions.create ~atoms:[] in
  let solver =
    Feel.Solver.create
      ~theory:(Feel.Theory.pack (module Uninterpreted_functions) theory)
      ()
  in
  { solver; theory; encoding = Formula.Encoding.create (); scopes = [] }
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
        Uninterpreted_functions.add_atom t.theory ~atom ~sat_var
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

let solve ?time_bound ?(assumptions = [||]) t =
  let scope_assumptions = Array.of_list t.scopes in
  let assumptions = Array.append scope_assumptions assumptions in
  Feel.Solver.solve ?time_bound ~assumptions t.solver
;;

let stats t = Feel.Solver.stats t.solver
