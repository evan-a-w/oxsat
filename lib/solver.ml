open! Core

module Sat_result = struct
  type t =
    | Sat of { assignments : Clause.t }
    | Unsat of { unsat_core : Clause.t }
  [@@deriving sexp]
end

type t =
  { mutable clauses : Clause.t list
  ; debug : bool
  }

let create ?(debug = false) () = { clauses = []; debug }

let add_clause t ~clause =
  t.clauses <- t.clauses @ [ clause ];
  t
;;

let add_clause' t ~clause = add_clause t ~clause:(Clause.of_int_array clause)

let create_with_formula ?debug formula =
  let t = create ?debug () in
  Array.iter formula ~f:(fun clause -> ignore (add_clause' t ~clause));
  t
;;

type clause_eval = [ `Sat | `Unsat | `Open | `Unit of int ]

let evaluate_clause (assignment : int array) (clause : int array) : clause_eval =
  let unassigned = ref None in
  let satisfied = ref false in
  Array.iter clause ~f:(fun lit ->
    let v = Int.abs lit in
    let a = assignment.(v) in
    if a = 0
    then (
      match !unassigned with
      | None -> unassigned := Some lit
      | Some _ -> unassigned := Some 0)
    else if (lit > 0 && a = 1) || (lit < 0 && a = -1)
    then satisfied := true);
  if !satisfied
  then `Sat
  else (
    match !unassigned with
    | None -> `Unsat
    | Some lit when Int.equal lit 0 -> `Open
    | Some lit -> `Unit lit)
;;

exception Conflict of int array

let propagate (assignment : int array) (clauses : int array list) : unit =
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter clauses ~f:(fun clause ->
      match evaluate_clause assignment clause with
      | `Sat | `Open -> ()
      | `Unsat -> raise (Conflict clause)
      | `Unit lit ->
        let v = Int.abs lit in
        let required = if lit > 0 then 1 else -1 in
        if assignment.(v) = 0
        then (
          assignment.(v) <- required;
          changed := true)
        else if assignment.(v) <> required
        then raise (Conflict clause))
  done
;;

let all_satisfied assignment clauses =
  List.for_all clauses ~f:(fun clause -> match evaluate_clause assignment clause with `Sat -> true | _ -> false)
;;

let choose_unassigned assignment =
  let rec go i =
    if i >= Array.length assignment
    then None
    else if assignment.(i) = 0
    then Some i
    else go (i + 1)
  in
  go 1
;;

let max_var_in_formula clauses assumptions =
  let from_clauses =
    List.fold clauses ~init:0 ~f:(fun acc clause ->
      Array.fold clause ~init:acc ~f:(fun acc lit -> Int.max acc (Int.abs lit)))
  in
  Array.fold assumptions ~init:from_clauses ~f:(fun acc lit -> Int.max acc (Int.abs lit))
;;

let assignments_clause assignment =
  let lits =
    Array.foldi assignment ~init:[] ~f:(fun i acc a ->
      if i = 0 || a = 0 then acc else (if a = 1 then i else -i) :: acc)
    |> List.rev
    |> Array.of_list
  in
  Clause.of_int_array lits
;;

let solve ?(assumptions = [||]) t =
  let _ = t.debug in
  let clauses = List.map t.clauses ~f:Clause.to_int_array in
  let clauses = Array.to_list (Array.map assumptions ~f:(fun lit -> [| lit |])) @ clauses in
  let max_var = max_var_in_formula clauses assumptions in
  let assignment = Array.create ~len:(max_var + 1) 0 in
  let rec search assignment =
    try
      propagate assignment clauses;
      if all_satisfied assignment clauses
      then Some (Array.copy assignment)
      else (
        match choose_unassigned assignment with
        | None -> Some (Array.copy assignment)
        | Some var ->
          let a_true = Array.copy assignment in
          a_true.(var) <- 1;
          (match search a_true with
           | Some _ as sat -> sat
           | None ->
             let a_false = Array.copy assignment in
             a_false.(var) <- -1;
             search a_false))
    with
    | Conflict _ -> None
  in
  match search assignment with
  | Some final_assignment -> Sat_result.Sat { assignments = assignments_clause final_assignment }
  | None -> Sat_result.Unsat { unsat_core = Clause.of_int_array [||] }
;;
