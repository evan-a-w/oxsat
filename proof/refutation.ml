open! Core
open! Theory_core

module Input_clause = struct
  type t =
    { input : int
    ; literal : Proof_literal.t
    }
  [@@deriving sexp, compare]
end

module Reason = struct
  type t =
    | Input_clause of Input_clause.t
    | Extension_definition of Proof_id.Extension.t
    | Theory_lemma of Proof_theory_certificate.t
    | Rup of { hints : Proof_id.Refutation_step.t array }
  [@@deriving sexp, compare]
end

module Step = struct
  type t =
    { clause : Proof_clause.t
    ; reason : Reason.t
    }
  [@@deriving sexp, compare]
end

type t =
  { inputs : Formula.any array
  ; extensions : Proof_extension.t array
  ; steps : Step.t array
  ; contradiction : Proof_id.Refutation_step.t
  }
[@@deriving sexp, compare]

let error message = Or_error.error_string message

let clause_is_singleton clause literal =
  match Proof_clause.literals clause with
  | [| actual |] -> [%compare.equal: Proof_literal.t] actual literal
  | _ -> false
;;

let validate_extensions extensions =
  let rec referenced_extensions formula =
    match (formula : Proof_boolean.t) with
    | True | False | Atom (Theory _) -> []
    | Atom (Extension id) -> [ Proof_id.Extension.to_int id ]
    | Not formula -> referenced_extensions formula
    | And formulas | Or formulas ->
      List.concat_map formulas ~f:referenced_extensions
  in
  Array.iteri extensions ~f:(fun index (extension : Proof_extension.t) ->
    let id = Proof_id.Extension.to_int extension.id in
    if id <> index
    then
      raise_s
        [%message
          "extension IDs must be dense and ordered" (index : int) (id : int)];
    List.iter (referenced_extensions extension.definition) ~f:(fun referenced ->
      if referenced >= index
      then
        raise_s
          [%message
            "extension definitions may only refer backwards"
              (index : int)
              (referenced : int)]))
;;

let check_input_clause t clause ({ input; literal } : Input_clause.t) =
  if input < 0 || input >= Array.length t.inputs
  then Or_error.error_s [%message "input index out of bounds" (input : int)]
  else if not (clause_is_singleton clause literal)
  then error "input clause must be the unit clause named by its certificate"
  else (
    let%bind.Or_error expected = Proof_boolean.of_formula t.inputs.(input) in
    let%bind.Or_error actual =
      Proof_boolean_check.of_literal literal
      |> Proof_boolean_check.expand_extensions ~extensions:t.extensions
    in
    if Proof_boolean_check.equivalent actual expected
    then Ok ()
    else error "input literal does not expand to its source formula")
;;

let check_extension_clause t clause id =
  let id_int = Proof_id.Extension.to_int id in
  if id_int < 0 || id_int >= Array.length t.extensions
  then
    Or_error.error_s [%message "extension index out of bounds" (id_int : int)]
  else (
    let extension = t.extensions.(id_int) in
    if not (Proof_id.Extension.equal extension.id id)
    then error "extension certificate ID does not match its definition"
    else (
      let atom = Proof_boolean.Atom (Proof_atom.Extension id) in
      let definition = extension.definition in
      let equivalence =
        Proof_boolean.And
          [ Or [ Not atom; definition ]; Or [ atom; Not definition ] ]
      in
      if Proof_boolean_check.entails
           ~assumptions:[ equivalence ]
           ~conclusion:(Proof_boolean_check.of_clause clause)
      then Ok ()
      else error "clause is not implied by its extension definition"))
;;

let assignment_find assignment atom =
  List.Assoc.find assignment atom ~equal:[%compare.equal: Proof_atom.t]
;;

let assign assignment atom value =
  match assignment_find assignment atom with
  | None -> `Ok ((atom, value) :: assignment)
  | Some existing when Bool.equal existing value -> `Ok assignment
  | Some _ -> `Conflict
;;

let check_rup steps ~step_index clause hints =
  let initial =
    Array.fold
      (Proof_clause.literals clause)
      ~init:(`Ok [])
      ~f:(fun state literal ->
        match state with
        | `Conflict -> `Conflict
        | `Ok assignment ->
          assign assignment literal.atom (not literal.positive))
  in
  match initial with
  | `Conflict -> Ok ()
  | `Ok initial ->
    let rec apply assignment = function
      | [] -> error "RUP hints did not derive a conflict"
      | hint :: hints ->
        let hint = Proof_id.Refutation_step.to_int hint in
        if hint < 0 || hint >= step_index
        then
          Or_error.error_s
            [%message "RUP hint is not an earlier step" (hint : int)]
        else (
          let hinted_clause = steps.(hint).Step.clause in
          let satisfied = ref false in
          let unassigned = ref [] in
          Array.iter (Proof_clause.literals hinted_clause) ~f:(fun literal ->
            match assignment_find assignment literal.atom with
            | Some value ->
              if Bool.equal value literal.positive then satisfied := true
            | None -> unassigned := literal :: !unassigned);
          if !satisfied
          then apply assignment hints
          else (
            match !unassigned with
            | [] -> Ok ()
            | [ literal ] ->
              (match assign assignment literal.atom literal.positive with
               | `Conflict -> Ok ()
               | `Ok assignment -> apply assignment hints)
            | _ -> error "RUP hint clause is not unit"))
    in
    apply initial (Array.to_list hints)
;;

let check t =
  Or_error.try_with_join (fun () ->
    validate_extensions t.extensions;
    let%bind.Or_error () =
      Array.foldi t.steps ~init:(Ok ()) ~f:(fun index result step ->
        let%bind.Or_error () = result in
        match step.Step.reason with
        | Input_clause input -> check_input_clause t step.clause input
        | Extension_definition id -> check_extension_clause t step.clause id
        | Theory_lemma certificate ->
          Proof_theory_certificate_check.check ~clause:step.clause certificate
        | Rup { hints } -> check_rup t.steps ~step_index:index step.clause hints)
    in
    let contradiction = Proof_id.Refutation_step.to_int t.contradiction in
    if contradiction < 0 || contradiction >= Array.length t.steps
    then
      Or_error.error_s
        [%message "contradiction step is out of bounds" (contradiction : int)]
    else if Array.is_empty
              (Proof_clause.literals t.steps.(contradiction).clause)
    then Ok ()
    else error "contradiction step is not the empty clause")
;;
