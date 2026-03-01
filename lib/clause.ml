open! Core
open! Import

module T = struct
  type t = int Vec.Value.t

  let create_for_pool () = Vec.Value.create ()

  let sexp_of_t t = [%sexp (Vec.Value.to_array t : int array)]
  let t_of_sexp sexp = Vec.Value.of_array_taking_ownership ([%of_sexp: int array] sexp)
end

include T

module Pool = Pool.Make (T)

let sort_compare a b =
  let c = Int.compare (Int.abs a) (Int.abs b) in
  if Int.equal c 0 then Int.compare a b else c
;;

let normalize_literals arr =
  let sorted = Array.filter arr ~f:(fun x -> not (Int.equal x 0)) in
  Array.sort sorted ~compare:sort_compare;
  Array.fold sorted ~init:[] ~f:(fun acc x ->
    match acc with
    | y :: _ when Int.equal x y -> acc
    | _ -> x :: acc)
  |> List.rev
  |> Array.of_list
;;

let of_int_array arr = Vec.Value.of_array_taking_ownership (normalize_literals (Array.copy arr))
let to_int_array t = Vec.Value.to_array t
let copy = Vec.Value.copy
let clear = Vec.Value.clear
let negate t = Vec.Value.map_inplace t ~f:Int.neg
let literals_list t = Vec.Value.to_list t

let iter_literals t ~f = Vec.Value.iter t ~f:(fun lit -> f (Literal.of_int lit))

let contains t ~var =
  let var = Int.abs var in
  Vec.Value.fold t ~init:false ~f:(fun acc lit -> acc || Int.equal (Int.abs lit) var)
;;

let contains_literal t ~literal =
  Vec.Value.fold t ~init:false ~f:(fun acc lit -> acc || Int.equal lit (Literal.to_int literal))
;;

let is_tautology t =
  let seen = Int.Table.create () in
  Vec.Value.fold t ~init:false ~f:(fun taut lit ->
    if taut
    then true
    else (
      let v = Int.abs lit in
      match Hashtbl.find seen v with
      | None ->
        Hashtbl.set seen ~key:v ~data:(lit > 0);
        false
      | Some b -> Bool.( <> ) b (lit > 0)))
;;

let literal_assignment assignments lit =
  let v = Int.abs lit in
  let pos = Bitset.get (Tf_pair.get assignments true) v in
  let neg = Bitset.get (Tf_pair.get assignments false) v in
  if pos
  then Some true
  else if neg
  then Some false
  else None
;;

let is_satisfied t ~assignments =
  Vec.Value.fold t ~init:false ~f:(fun acc lit ->
    acc
    ||
    match literal_assignment assignments lit with
    | Some b -> Bool.equal b (lit > 0)
    | None -> false)
;;

let unit_literal t ~assignments =
  if is_satisfied t ~assignments
  then Literal.Option.none ()
  else (
    let unknown = ref None in
    let many_unknown = ref false in
    Vec.Value.iter t ~f:(fun lit ->
      match literal_assignment assignments lit with
      | Some _ -> ()
      | None ->
        (match !unknown with
         | None -> unknown := Some lit
         | Some _ -> many_unknown := true));
    match !unknown, !many_unknown with
    | Some lit, false -> Literal.Option.some (Literal.of_int lit)
    | _ -> Literal.Option.none ())
;;

let can_resolve t ~other ~on_var =
  let has_pos clause = Vec.Value.fold clause ~init:false ~f:(fun acc lit -> acc || Int.equal lit on_var) in
  let has_neg clause = Vec.Value.fold clause ~init:false ~f:(fun acc lit -> acc || Int.equal lit (-on_var)) in
  (has_pos t && has_neg other) || (has_neg t && has_pos other)
;;

let resolve_exn t ~other ~on_var =
  if not (can_resolve t ~other ~on_var)
  then failwith "Clause.resolve_exn: cannot resolve"
  else (
    let out = Int.Table.create () in
    let add lit =
      if Int.equal (Int.abs lit) (Int.abs on_var)
      then ()
      else Hashtbl.set out ~key:lit ~data:()
    in
    Vec.Value.iter t ~f:add;
    Vec.Value.iter other ~f:add;
    let merged = Hashtbl.keys out |> List.sort ~compare:sort_compare |> Array.of_list in
    clear t;
    Array.iter merged ~f:(Vec.Value.push t))
;;
