open! Core
open! Import

type t =
  { present : int Vec.Value.t
  ; location_by_literal :
      (* -1 is none because or null is annoying and cbf to make an optional int thing *)
      int Vec.Value.t Tf_pair.t
  ; random_state : Random.State.t
  }

let create () =
  { present = Vec.Value.create ()
  ; location_by_literal = Tf_pair.create (fun _ -> Vec.Value.create ())
  ; random_state = Random.State.default
  }
;;

let location t ~literal =
  match
    Vec.Value.get
      (Tf_pair.get t.location_by_literal (literal > 0))
      (Int.abs literal)
  with
  | -1 -> Null
  | i -> This i
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
    if Vec.Value.length t.present > 1
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
