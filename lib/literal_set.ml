open! Core
open! Import

type t =
  { present : int Vec.Value.t
  ; location_by_literal :
      (* -1 is none because or null is annoying and cbf to make an optional int thing *)
      int Vec.Value.t Tf_pair.t
  ; random_state : Random.State.t
  }

let create ~random_state =
  { present = Vec.Value.create ()
  ; location_by_literal = Tf_pair.create (fun _ -> Vec.Value.create ())
  ; random_state
  }
;;

let location t ~literal =
  match
    Vec.Value.get_opt
      (Tf_pair.get t.location_by_literal (literal > 0))
      (Int.abs literal)
  with
  | None -> Null
  | Some location ->
    (match location with
     | -1 -> Null
     | i -> This i)
;;

let set_location t ~literal ~location =
  let location =
    match location with
    | Null -> -1
    | This i -> i
  in
  Vec.Value.set
    (Tf_pair.get t.location_by_literal (literal > 0))
    (Int.abs literal)
    location
;;

let swap t idx1 idx2 =
  let literal1 = Vec.Value.get t.present idx1 in
  let literal2 = Vec.Value.get t.present idx2 in
  let location1 = location t ~literal:literal1 in
  let location2 = location t ~literal:literal2 in
  Vec.Value.swap t.present idx1 idx2;
  set_location t ~literal:literal1 ~location:location2;
  set_location t ~literal:literal2 ~location:location1
;;

let insert t ~literal =
  match location t ~literal with
  | This _ -> ()
  | Null ->
    Vec.Value.push t.present literal;
    let loc_vec = Tf_pair.get t.location_by_literal (literal > 0) in
    Vec.Value.fill_to_length
      loc_vec
      ~length:(Int.abs literal + 1)
      ~f:(fun (_ : int) -> -1);
    Vec.Value.set loc_vec (Int.abs literal) (Vec.Value.length t.present - 1);
    let random_pos =
      Random.State.int t.random_state (Vec.Value.length t.present)
    in
    swap t (Vec.Value.length t.present - 1) random_pos
;;

let remove t ~literal =
  match location t ~literal with
  | Null -> ()
  | This i ->
    set_location t ~literal ~location:Null;
    if Vec.Value.length t.present > 1 && Vec.Value.last_exn t.present <> literal
    then (
      let to_swap = Vec.Value.last_exn t.present in
      set_location t ~literal:to_swap ~location:(This i);
      Vec.Value.swap t.present i (Vec.Value.length t.present - 1));
    ignore (Vec.Value.pop_exn t.present : int)
;;

let pop_one t =
  if Vec.Value.length t.present = 0
  then Null
  else (
    let literal = Vec.Value.pop_exn t.present in
    set_location t ~literal ~location:Null;
    This literal)
;;

let sorted_literals t =
  Vec.Value.to_list t.present |> List.sort ~compare:Int.compare
;;

let location_opt t ~literal =
  match location t ~literal with
  | Null -> None
  | This i -> Some i
;;

let invariant_holds t =
  let ok = ref true in
  Vec.Value.iteri t.present ~f:(fun i literal ->
    if not (Option.equal Int.equal (location_opt t ~literal) (Some i))
    then ok := false);
  Tf_pair.iter t.location_by_literal ~f:(fun locations ->
    Vec.Value.iteri locations ~f:(fun _ location ->
      if location <> -1
      then (
        match Vec.Value.get_opt t.present location with
        | Some _ -> ()
        | None -> ok := false)));
  !ok
;;

let drain_sorted t =
  let rec go acc =
    match pop_one t with
    | Null -> List.sort acc ~compare:Int.compare
    | This literal -> go (literal :: acc)
  in
  go []
;;

let location_round_trips t literals =
  List.map literals ~f:(fun literal ->
    let round_trip =
      match location_opt t ~literal with
      | None -> None
      | Some location -> Vec.Value.get_opt t.present location
    in
    literal, round_trip)
;;

let%expect_test "insert tracks literals and reverse locations" =
  let t = create ~random_state:(Random.State.make [| 1; 2; 3 |]) in
  List.iter [ 3; -2; 7; -5 ] ~f:(fun literal -> insert t ~literal);
  print_s
    [%message
      (sorted_literals t : int list)
        ~round_trips:
          (location_round_trips t [ -5; -3; -2; 2; 3; 7 ]
           : (int * int option) list)
        ~invariant_holds:(invariant_holds t : bool)];
  [%expect
    {|
    (("sorted_literals t" (-5 -2 3 7))
     (round_trips ((-5 (-5)) (-3 ()) (-2 (-2)) (2 ()) (3 (3)) (7 (7))))
     (invariant_holds true))
    |}]
;;

let%expect_test "insert is idempotent and remove is tolerant" =
  let t = create ~random_state:(Random.State.make [| 1; 2; 3 |]) in
  List.iter [ 4; 4; -4; 4; -4 ] ~f:(fun literal -> insert t ~literal);
  print_s
    [%message
      "after inserts"
        (sorted_literals t : int list)
        ~invariant_holds:(invariant_holds t : bool)];
  remove t ~literal:4;
  print_s
    [%message
      "after removing positive"
        (sorted_literals t : int list)
        ~round_trips:
          (location_round_trips t [ -4; 4 ] : (int * int option) list)
        ~invariant_holds:(invariant_holds t : bool)];
  remove t ~literal:4;
  remove t ~literal:17;
  print_s
    [%message
      "after removing missing literals"
        (sorted_literals t : int list)
        ~invariant_holds:(invariant_holds t : bool)];
  [%expect
    {|
    ("after inserts" ("sorted_literals t" (-4 4)) (invariant_holds true))
    ("after removing positive" ("sorted_literals t" (-4))
     (round_trips ((-4 (-4)) (4 ()))) (invariant_holds true))
    ("after removing missing literals" ("sorted_literals t" (-4))
     (invariant_holds true))
    |}]
;;

let%expect_test "remove updates moved literal location" =
  let t = create ~random_state:(Random.State.make [| 1; 2; 3 |]) in
  List.iter [ 1; 2; 3; 4; 5; 6 ] ~f:(fun literal -> insert t ~literal);
  remove t ~literal:3;
  remove t ~literal:6;
  print_s
    [%message
      (sorted_literals t : int list)
        ~round_trips:
          (location_round_trips t [ 1; 2; 3; 4; 5; 6 ]
           : (int * int option) list)
        ~invariant_holds:(invariant_holds t : bool)];
  [%expect
    {|
    (("sorted_literals t" (1 2 4 5))
     (round_trips ((1 (1)) (2 (2)) (3 ()) (4 (4)) (5 (5)) (6 ())))
     (invariant_holds true))
    |}]
;;

let%expect_test "pop_one returns each literal once and empties the set" =
  let t = create ~random_state:(Random.State.make [| 1; 2; 3 |]) in
  List.iter [ -10; 1; 4; -3; 8 ] ~f:(fun literal -> insert t ~literal);
  let popped = drain_sorted t in
  let remaining = sorted_literals t in
  let pop_empty =
    match pop_one t with
    | Null -> true
    | This _ -> false
  in
  print_s
    [%message
      "drained"
        (popped : int list)
        (remaining : int list)
        (pop_empty : bool)
        ~invariant_holds:(invariant_holds t : bool)];
  [%expect
    {|
    (drained (popped (-10 -3 1 4 8)) (remaining ()) (pop_empty true)
     (invariant_holds true))
    |}]
;;
