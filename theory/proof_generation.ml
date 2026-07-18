open! Core
open! Import

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

let formulas t =
  List.rev t.scopes |> List.concat_map ~f:List.rev |> Array.of_list
;;

let unsat_proof t =
  let formulas = formulas t in
  let assumptions : Proof.Assumption.t array =
    Array.map formulas ~f:(fun formula ->
      { Proof.Assumption.name = None; formula })
  in
  let assumption_steps : Proof.Step.t array =
    Array.mapi formulas ~f:(fun index conclusion ->
      { Proof.Step.name = None
      ; conclusion
      ; justification = Assumption (Proof.Id.Assumption.of_int_exn index)
      })
  in
  let premise_ids =
    Array.mapi formulas ~f:(fun index _ -> Proof.Id.Step.of_int_exn index)
  in
  let final_step : Proof.Step.t =
    { Proof.Step.name = None
    ; conclusion = Formula.False
    ; justification = Kernel { rule = Propositional; premises = premise_ids }
    }
  in
  let proof : Proof.t =
    { assumptions
    ; steps = Array.append assumption_steps [| final_step |]
    ; conclusion = Proof.Id.Step.of_int_exn (Array.length formulas)
    }
  in
  match Proof.check proof with
  | Ok () -> Some proof
  | Error _ -> None
;;
