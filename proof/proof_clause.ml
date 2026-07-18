open! Core

type t = Proof_literal.t array [@@deriving sexp_of, compare]

let create literals =
  let literals = Array.of_list literals in
  Array.sort literals ~compare:Proof_literal.compare;
  let rec deduplicate reversed i =
    if i = Array.length literals
    then `Clause (Array.of_list_rev reversed)
    else (
      let literal = literals.(i) in
      match reversed with
      | previous :: _
        when [%compare.equal: Proof_atom.t]
               previous.Proof_literal.atom
               literal.Proof_literal.atom ->
        if Bool.equal previous.positive literal.positive
        then deduplicate reversed (i + 1)
        else `Tautology
      | _ -> deduplicate (literal :: reversed) (i + 1))
  in
  deduplicate [] 0
;;

let t_of_sexp sexp =
  match create ([%of_sexp: Proof_literal.t array] sexp |> Array.to_list) with
  | `Clause clause -> clause
  | `Tautology -> of_sexp_error "a proof clause cannot be tautological" sexp
;;

let empty = [||]
let literals t = Array.copy t
