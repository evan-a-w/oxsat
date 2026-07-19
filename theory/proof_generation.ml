open! Core
open! Import
module Refutation_clause = Feel.Solver.Refutation_clause

type t = { mutable scopes : Formula.any list list }

let create () = { scopes = [ [] ] }

let assert_formula t formula =
  match t.scopes with
  | current :: outer -> t.scopes <- (formula :: current) :: outer
  | [] -> assert false
;;

let push t = t.scopes <- [] :: t.scopes

let pop t =
  match t.scopes with
  | _ :: (_ :: _ as outer) -> t.scopes <- outer
  | _ -> assert false
;;

let asserted_formulas t =
  List.rev t.scopes |> List.concat_map ~f:List.rev |> Array.of_list
;;

(* Resolves SAT variables and literals to their proof-level counterparts and
   assigns dense extension IDs to Tseitin auxiliaries in dependency order. *)
module Resolver = struct
  type t =
    { encoding : Encoding.t
    ; extension_id_by_var : int Int.Table.t
    ; extensions : Proof.Extension.t Queue.t
    }

  let create encoding =
    { encoding
    ; extension_id_by_var = Int.Table.create ()
    ; extensions = Queue.create ()
    }
  ;;

  (* Ensures [var]'s extension (and, recursively, those it depends on) has an ID
     and definition, returning its [Proof.Id.Extension.t]. Tseitin vars are
     created bottom-up, so a definition only ever refers to smaller vars, which
     are registered first -- keeping extension IDs backward-referring. *)
  let rec extension_id t var =
    match Hashtbl.find t.extension_id_by_var var with
    | Some id -> Proof.Id.Extension.of_int_exn id
    | None ->
      let definition =
        match Encoding.tseitin_def t.encoding var with
        | Some (And literals) ->
          Proof.Boolean.And (List.map literals ~f:(boolean_of_literal t))
        | Some (Or literals) ->
          Proof.Boolean.Or (List.map literals ~f:(boolean_of_literal t))
        | None ->
          raise_s
            [%message
              "expected a Tseitin definition for extension var" (var : int)]
      in
      let id = Queue.length t.extensions in
      Hashtbl.set t.extension_id_by_var ~key:var ~data:id;
      Queue.enqueue
        t.extensions
        { Proof.Extension.id = Proof.Id.Extension.of_int_exn id; definition };
      Proof.Id.Extension.of_int_exn id

  and atom_of_var t var : Proof.Atom.t =
    match Encoding.atom_for_sat_var t.encoding var with
    | Some atom -> Theory atom
    | None -> Extension (extension_id t var)

  and boolean_of_literal t literal : Proof.Boolean.t =
    let atom = Proof.Boolean.Atom (atom_of_var t (Int.abs literal)) in
    if literal > 0 then atom else Not atom
  ;;

  let literal t literal : Proof.Literal.t =
    Proof.Literal.create
      ~atom:(atom_of_var t (Int.abs literal))
      ~positive:(literal > 0)
  ;;

  let clause t literals =
    match
      Proof.Clause.create (Array.to_list literals |> List.map ~f:(literal t))
    with
    | `Clause clause -> clause
    | `Tautology -> raise_s [%message "refutation clause is tautological"]
  ;;

  let extensions t = Queue.to_array t.extensions
end

(* Computes a RUP hint order for [clause] by simulating unit propagation over
   [prior_clauses] (all steps emitted before this one), exactly as the
   refutation checker does. Starting from the negation of [clause]'s literals,
   it repeatedly fires any prior clause that is unit under the current
   assignment until a conflict arises, returning the fired clauses' indices in
   order. This is robust: it never cites a non-unit clause, and finding a firing
   order is a fixed point independent of how the SAT core happened to resolve
   the clause. *)
let rup_hints ~clause ~prior_clauses =
  let assignment = ref [] in
  let find atom =
    List.Assoc.find !assignment atom ~equal:[%compare.equal: Proof.Atom.t]
  in
  let set atom value = assignment := (atom, value) :: !assignment in
  let conflict = ref false in
  Array.iter
    (Proof.Clause.literals clause)
    ~f:(fun (literal : Proof.Literal.t) ->
      match find literal.atom with
      | Some existing when not (Bool.equal existing (not literal.positive)) ->
        conflict := true
      | _ -> set literal.atom (not literal.positive));
  let hints = Queue.create () in
  let progress = ref true in
  while (not !conflict) && !progress do
    progress := false;
    Array.iteri prior_clauses ~f:(fun index (prior : Proof.Clause.t) ->
      if not !conflict
      then (
        let satisfied = ref false in
        let unassigned = ref [] in
        Array.iter
          (Proof.Clause.literals prior)
          ~f:(fun (lit : Proof.Literal.t) ->
            match find lit.atom with
            | Some value ->
              if Bool.equal value lit.positive then satisfied := true
            | None -> unassigned := lit :: !unassigned);
        if not !satisfied
        then (
          match !unassigned with
          | [] ->
            Queue.enqueue hints index;
            conflict := true
          | [ lit ] ->
            Queue.enqueue hints index;
            set lit.atom lit.positive;
            progress := true
          | _ -> ())))
  done;
  if not !conflict
  then
    raise_s [%message "could not find a RUP derivation for a refutation clause"];
  Queue.to_array hints
;;

let build
  ~encoding
  ~certificate_for_atoms
  ~formula_by_root_lit
  ~refutation_clauses
  ~inputs
  =
  let resolver = Resolver.create encoding in
  let input_index = Formula.Any.Table.create () in
  Array.iteri inputs ~f:(fun index formula ->
    if not (Hashtbl.mem input_index formula)
    then Hashtbl.set input_index ~key:formula ~data:index);
  let steps = Queue.create () in
  let atoms_of_literals literals =
    Array.to_list literals
    |> List.filter_map ~f:(fun literal ->
      Encoding.atom_for_sat_var encoding (Int.abs literal))
  in
  List.iter refutation_clauses ~f:(fun (rc : Refutation_clause.t) ->
    let clause = Resolver.clause resolver rc.literals in
    let reason : Proof.Refutation.Reason.t =
      match rc.reason with
      | Theory ->
        let atoms = atoms_of_literals rc.literals in
        (match certificate_for_atoms atoms with
         | Some certificate ->
           Theory_lemma
             (Lemma_certificate.to_theory_certificate clause certificate)
         | None ->
           raise_s [%message "theory clause without a recorded certificate"])
      | Rup ->
        let prior_clauses =
          Queue.to_array steps
          |> Array.map ~f:(fun (step : Proof.Refutation.Step.t) -> step.clause)
        in
        Rup
          { hints =
              rup_hints ~clause ~prior_clauses
              |> Array.map ~f:Proof.Id.Refutation_step.of_int_exn
          }
      | Input ->
        (* A single-literal User clause is the unit assertion of a top-level
           formula (its literal names a Tseitin root or a bare atom); a
           multi-literal one is a Tseitin definition clause. *)
        (match rc.literals with
         | [| lit |] ->
           let formula =
             match Hashtbl.find formula_by_root_lit lit with
             | Some formula -> formula
             | None ->
               raise_s
                 [%message
                   "unit input clause has no source formula" (lit : int)]
           in
           let input =
             match Hashtbl.find input_index formula with
             | Some input -> input
             | None ->
               raise_s
                 [%message "unit input clause's formula is not an assumption"]
           in
           Input_clause { input; literal = Resolver.literal resolver lit }
         | _ ->
           let def_var =
             Array.find_map rc.literals ~f:(fun lit ->
               let var = Int.abs lit in
               Option.some_if
                 (Option.is_some (Encoding.tseitin_def encoding var))
                 var)
           in
           (match def_var with
            | Some var ->
              Extension_definition (Resolver.extension_id resolver var)
            | None ->
              raise_s
                [%message
                  "multi-literal input clause is neither an assertion nor a \
                   Tseitin definition"
                    ~literals:(rc.literals : int array)]))
    in
    Queue.enqueue steps { Proof.Refutation.Step.clause; reason });
  let steps = Queue.to_array steps in
  let contradiction =
    Proof.Id.Refutation_step.of_int_exn (Array.length steps - 1)
  in
  { Proof.Refutation.inputs
  ; extensions = Resolver.extensions resolver
  ; steps
  ; contradiction
  }
;;

let unsat_proof
  t
  ~encoding
  ~certificate_for_atoms
  ~formula_by_root_lit
  ~scope_vars
  ~refutation_clauses
  =
  (* Proofs of assertions made inside a [push]/[pop] scope would have to account
     for the scope's activation literal (which the SAT core assumes true but
     which has no formula); that is not yet modeled, so decline to produce a
     proof when the refutation touches one. *)
  let scope_vars = Int.Set.of_list scope_vars in
  let touches_scope_var =
    List.exists refutation_clauses ~f:(fun (rc : Refutation_clause.t) ->
      Array.exists rc.literals ~f:(fun lit -> Set.mem scope_vars (Int.abs lit)))
  in
  if touches_scope_var
  then None
  else (
    let premises = asserted_formulas t in
    (* The refutation refutes the premises together with the negation of the
       step's conclusion ([False]); [Proof.check] requires exactly this input
       layout. *)
    let inputs = Array.append premises [| Formula.Not Formula.False |] in
    let refutation =
      build
        ~encoding
        ~certificate_for_atoms
        ~formula_by_root_lit
        ~refutation_clauses
        ~inputs
    in
    let assumptions : Proof.Assumption.t array =
      Array.map premises ~f:(fun formula ->
        { Proof.Assumption.name = None; formula })
    in
    let assumption_steps : Proof.Step.t array =
      Array.mapi premises ~f:(fun index conclusion ->
        { Proof.Step.name = None
        ; conclusion
        ; justification = Assumption (Proof.Id.Assumption.of_int_exn index)
        })
    in
    let premise_ids =
      Array.mapi premises ~f:(fun index _ -> Proof.Id.Step.of_int_exn index)
    in
    let final_step : Proof.Step.t =
      { Proof.Step.name = None
      ; conclusion = Formula.False
      ; justification = By_refutation { premises = premise_ids; refutation }
      }
    in
    let proof : Proof.t =
      { assumptions
      ; steps = Array.append assumption_steps [| final_step |]
      ; conclusion = Proof.Id.Step.of_int_exn (Array.length premises)
      }
    in
    match Proof.check proof with
    | Ok () -> Some proof
    | Error error ->
      raise_s [%message "generated proof failed to check" (error : Error.t)])
;;
