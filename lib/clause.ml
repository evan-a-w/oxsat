open! Core
open! Import

type t =
  { mutable lits : int array
  }
[@@deriving sexp]

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

let of_int_array arr = { lits = normalize_literals (Array.copy arr) }
let to_int_array t = Array.copy t.lits
let copy t = { lits = Array.copy t.lits }
let clear t = t.lits <- [||]

let negate t =
  for i = 0 to Array.length t.lits - 1 do
    t.lits.(i) <- -t.lits.(i)
  done
;;

let literals_list t = Array.to_list t.lits

let iter_literals t ~f = Array.iter t.lits ~f:(fun lit -> f (Literal.of_int lit))

let contains t ~var =
  let var = Int.abs var in
  Array.exists t.lits ~f:(fun lit -> Int.equal (Int.abs lit) var)
;;

let contains_literal t ~literal =
  Array.exists t.lits ~f:(fun lit -> Int.equal lit (Literal.to_int literal))
;;

let is_tautology t =
  let seen = Int.Table.create () in
  Array.exists t.lits ~f:(fun lit ->
    let v = Int.abs lit in
    match Hashtbl.find seen v with
    | None ->
      Hashtbl.set seen ~key:v ~data:(lit > 0);
      false
    | Some b -> Bool.( <> ) b (lit > 0))
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
  Array.exists t.lits ~f:(fun lit ->
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
    Array.iter t.lits ~f:(fun lit ->
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
  let has_pos clause = Array.exists clause.lits ~f:(fun lit -> Int.equal lit on_var) in
  let has_neg clause = Array.exists clause.lits ~f:(fun lit -> Int.equal lit (-on_var)) in
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
    Array.iter t.lits ~f:add;
    Array.iter other.lits ~f:add;
    t.lits <-
      Hashtbl.keys out
      |> List.sort ~compare:sort_compare
      |> Array.of_list)
;;

module Pool = struct
  module P = Pool.Make (struct
      type nonrec t = t

      let create_for_pool () = of_int_array [||]
    end)

  type pool = P.t

  let create = P.create
  let alloc = P.alloc
  let free = P.free
  let get = P.get
  let set = P.set
  let iter = P.iter
  let outstanding = P.outstanding
end
