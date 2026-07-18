open! Core

let of_literal (literal : Proof_literal.t) =
  let atom = Proof_boolean.Atom literal.atom in
  if literal.positive then atom else Proof_boolean.Not atom
;;

let of_clause clause =
  Proof_boolean.Or
    (Proof_clause.literals clause |> Array.to_list |> List.map ~f:of_literal)
;;

let rec atoms formula =
  match (formula : Proof_boolean.t) with
  | True | False -> []
  | Atom atom -> [ atom ]
  | Not formula -> atoms formula
  | And formulas | Or formulas -> List.concat_map formulas ~f:atoms
;;

let rec eval formula ~assignment =
  match (formula : Proof_boolean.t) with
  | True -> true
  | False -> false
  | Atom atom ->
    List.Assoc.find_exn assignment atom ~equal:(fun a b ->
      Proof_atom.compare a b = 0)
  | Not formula -> not (eval formula ~assignment)
  | And formulas -> List.for_all formulas ~f:(eval ~assignment)
  | Or formulas -> List.exists formulas ~f:(eval ~assignment)
;;

let entails ~assumptions ~conclusion =
  let atoms =
    List.concat_map (conclusion :: assumptions) ~f:atoms
    |> List.dedup_and_sort ~compare:Proof_atom.compare
  in
  let rec check assignment = function
    | [] ->
      (not (List.for_all assumptions ~f:(eval ~assignment)))
      || eval conclusion ~assignment
    | atom :: atoms ->
      check ((atom, false) :: assignment) atoms
      && check ((atom, true) :: assignment) atoms
  in
  check [] atoms
;;

let equivalent left right =
  entails ~assumptions:[ left ] ~conclusion:right
  && entails ~assumptions:[ right ] ~conclusion:left
;;

let expand_extensions ~extensions formula =
  let%bind.Or_error definitions =
    Array.mapi extensions ~f:(fun expected (extension : Proof_extension.t) ->
      let actual = Proof_id.Extension.to_int extension.id in
      if actual = expected
      then Ok extension.definition
      else
        Or_error.error_s
          [%message
            "extension IDs must be dense and ordered"
              (expected : int)
              (actual : int)])
    |> Array.to_list
    |> Or_error.all
    |> Or_error.map ~f:Array.of_list
  in
  let rec expand visiting = function
    | Proof_boolean.True -> Ok Proof_boolean.True
    | False -> Ok False
    | Atom (Theory _ as atom) -> Ok (Atom atom)
    | Atom (Extension id) ->
      let id = Proof_id.Extension.to_int id in
      if Set.mem visiting id
      then Or_error.error_s [%message "cyclic extension definition" (id : int)]
      else if id < 0 || id >= Array.length definitions
      then Or_error.error_s [%message "unknown extension" (id : int)]
      else expand (Set.add visiting id) definitions.(id)
    | Not formula ->
      let%map.Or_error formula = expand visiting formula in
      Proof_boolean.Not formula
    | And formulas ->
      let%map.Or_error formulas =
        Or_error.all (List.map formulas ~f:(expand visiting))
      in
      Proof_boolean.And formulas
    | Or formulas ->
      let%map.Or_error formulas =
        Or_error.all (List.map formulas ~f:(expand visiting))
      in
      Proof_boolean.Or formulas
  in
  expand Int.Set.empty formula
;;
