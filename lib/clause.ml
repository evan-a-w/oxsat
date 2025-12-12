open! Core
open! Import

module T = struct
  type t = int Vec.Value.t
  [@@deriving sexp]


  let create_for_pool () = Vec.Value.create ()
end

module Pool = Pool.Make [@kind value] (T)
include T

let negate t = Vec.Value.map_inplace t ~f:(Int.neg)
let copy t = Vec.Value.copy t

let is_tautology t =
  let open Local_ref.O in
  let seen_taut = Local_ref.create false in
  for i = 1 to Vec.Value.length t - 1 do
    if Vec.Value.get t i = -Vec.Value.get t (i - 1) then seen_taut := true
  done;
  !seen_taut [@nontail]
;;

let is_satisfied t ~assignments =
  Vec.Value.fold t ~init:false ~f:(fun acc literal ->
    acc || Bitset.get (Tf_pair.get assignments (literal > 0)) (Int.abs literal))
;;

let iter_literals t ~(local_ f) =
  Vec.Value.iter t ~f:(fun x -> f (Literal.of_int x)) [@nontail]
;;

let%template literals_list (t : t) : int list @ m =
  let rec go i acc =
    if i = Vec.Value.length t then acc else go (i + 1) (Vec.Value.get t i :: acc)
  in
  let res = go 0 [] in
  (List.rev [@alloc a]) res [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let contains t ~var =
  let open Local_ref.O in
  let seen = Local_ref.create false in
  Vec.Value.iter t ~f:(fun literal ->
    if Int.abs literal = var then seen := true);
  !seen [@nontail]
;;

let sort_compare a b = if Int.abs a = Int.abs b then Int.compare a b else Int.compare (Int.abs a) (Int.abs b)

let contains_literal t ~literal =
  let literal = Literal.to_int literal in
  match Vec.Value.binary_search t ~f:(fun literal' -> sort_compare literal' literal) ~which:`First_equal with
  | None -> false
  | Some _ -> true
;;

let unit_literal t ~assignments =
  if is_satisfied t ~assignments
  then Literal.Option.none ()
  else (
    let res =
      (Vec.Value.fold [@mode local])
        t
        ~init:(Ok None)
        ~f:(fun acc literal -> exclave_
          match acc with
          | Error _ -> acc
          | Ok acc ->
            if Bitset.get
                 (Tf_pair.get assignments (literal < 0))
                 (Int.abs literal)
            then Ok acc
            else (
              match acc with
              | None -> Ok (Some literal)
              | Some _ -> Error ()))
    in
    match res with
    | Ok (Some literal) -> Literal.Option.some (Literal.of_int literal)
    | Ok None | Error _ -> Literal.Option.none ())
;;

let clear = Vec.Value.clear

let can_resolve t ~other ~on_var =
  let open Local_ref.O in
  let t_v = Tf_pair.create_local (fun _ -> exclave_ Local_ref.create false) in
  let other_v =
    Tf_pair.create_local (fun _ -> exclave_ Local_ref.create false)
  in
  let update clause v = exclave_
      iter_literals clause ~f:(fun lit ->
      if Literal.var lit = on_var
      then (
        let r = (Tf_pair.get [@mode local]) v (Literal.value lit) in
        (r := true) [@nontail]))
  in
  update t t_v;
  update other other_v;
  let try_ v =
    !((Tf_pair.get [@mode local]) t_v v) && !((Tf_pair.get [@mode local]) other_v (not v)) [@nontail]
  in
  try_ true || try_ false [@nontail]
;;

let of_int_array arr =
  Array.sort arr ~compare:sort_compare;
  Vec.Value.of_array_taking_ownership arr
;;

let to_int_array t = Vec.Value.to_array t

let resolve_exn t ~other ~on_var =
  if not (can_resolve t ~other ~on_var)
  then
    Error.raise_s
      [%message
        "Can't resolve clauses"
          (on_var : int)
          ~t:(to_int_array t : int array)
          ~other:(to_int_array other : int array)]
  else (
    let old_len = Vec.Value.length t in
    Vec.Value.iter other ~f:(fun x ->
      if Int.abs x <> on_var then
        match Vec.Value.binary_search ~end_:old_len t ~f:(fun y -> sort_compare y x) ~which:`First_equal with
        | Some _ -> ()
        | None -> Vec.Value.push t x);
    Vec.Value.sort_partitioned t ~a_len:old_len ~compare:sort_compare;
    Vec.Value.filter_inplace t ~f:(fun x -> Int.abs x <> on_var))
;;

let%expect_test "unit literal detects single remaining negative" =
  let clause = of_int_array [| -1; -4; -6 |] in
  let assignments = Tf_pair.create (fun (_ : bool) -> Bitset.create ()) in
  Bitset.set (Tf_pair.get assignments true) 4;
  Bitset.set (Tf_pair.get assignments true) 6;
  (match%optional_u (unit_literal clause ~assignments : Literal.Option.t) with
   | None -> print_endline "None"
   | Some literal -> Literal.to_int literal |> Int.to_string |> print_endline);
  [%expect {| -1 |}]
;;
