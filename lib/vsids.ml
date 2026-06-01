open! Core
open! Import

module Heap = struct
  type t =
    { vars : int Vec.Value.t
    ; activity_by_var : float Vec.Value.t
    ; pos_by_var : int Vec.Value.t
    ; mutable length : int
    }

  let create () =
    { vars = Vec.Value.create ()
    ; activity_by_var = Vec.Value.create ()
    ; pos_by_var = Vec.Value.create ()
    ; length = 0
    }
  ;;

  let ensure_var_capacity t ~var =
    Vec.Value.fill_to_length t.activity_by_var ~length:(var + 1) ~f:(fun _ ->
      0.);
    Vec.Value.fill_to_length t.pos_by_var ~length:(var + 1) ~f:(fun _ -> -1)
  ;;

  let compare_at t i j =
    let var_i = Vec.Value.get t.vars i in
    let var_j = Vec.Value.get t.vars j in
    match
      Float.compare
        (Vec.Value.get t.activity_by_var var_i)
        (Vec.Value.get t.activity_by_var var_j)
    with
    | 0 -> Int.compare var_i var_j
    | o -> o
  ;;

  let better_at t i j = compare_at t i j > 0

  let swap t i j =
    let var_i = Vec.Value.get t.vars i in
    let var_j = Vec.Value.get t.vars j in
    Vec.Value.set t.vars i var_j;
    Vec.Value.set t.vars j var_i;
    Vec.Value.set t.pos_by_var var_i j;
    Vec.Value.set t.pos_by_var var_j i
  ;;

  let rec bubble_up t idx =
    if idx > 0
    then (
      let parent = (idx - 1) / 2 in
      if better_at t idx parent
      then (
        swap t idx parent;
        bubble_up t parent))
  ;;

  let rec bubble_down t idx =
    let left = (idx * 2) + 1 in
    if left < t.length
    then (
      let right = left + 1 in
      let best =
        if right < t.length && better_at t right left then right else left
      in
      if better_at t best idx
      then (
        swap t idx best;
        bubble_down t best))
  ;;

  let push t var =
    ensure_var_capacity t ~var;
    Vec.Value.push t.vars var;
    Vec.Value.set t.pos_by_var var t.length;
    t.length <- t.length + 1;
    bubble_up t (t.length - 1)
  ;;

  let peek_var t = if t.length = 0 then None else Some (Vec.Value.get t.vars 0)

  let remove_at t idx =
    let removed_var = Vec.Value.get t.vars idx in
    let last_idx = t.length - 1 in
    let last_var = Vec.Value.pop_exn t.vars in
    t.length <- last_idx;
    Vec.Value.set t.pos_by_var removed_var (-1);
    if idx < t.length
    then (
      Vec.Value.set t.vars idx last_var;
      Vec.Value.set t.pos_by_var last_var idx;
      let parent = (idx - 1) / 2 in
      if idx > 0 && better_at t idx parent
      then bubble_up t idx
      else bubble_down t idx)
  ;;

  let bump_activity t ~var ~amount =
    ensure_var_capacity t ~var;
    Vec.Value.set
      t.activity_by_var
      var
      (Vec.Value.get t.activity_by_var var +. amount)
  ;;

  let rescale_activities t ~scale =
    Vec.Value.map_inplace t.activity_by_var ~f:(fun activity ->
      activity /. scale)
  ;;

  let activity t var = Vec.Value.get t.activity_by_var var

  let fix_after_bump t ~var =
    let idx = Vec.Value.get t.pos_by_var var in
    if idx >= 0 then bubble_up t idx
  ;;
end

type t =
  { heap : Heap.t
  ; known_vars : Bitset.t
  ; in_pool : Bitset.t
  ; in_heap : Bitset.t
  (* per-polarity activity: pos_activity tracks positive literals, heap tracks total *)
  ; pos_activity : float Vec.Value.t
  ; neg_activity : float Vec.Value.t
  ; mutable activity_inc : float
  }

let create () =
  { heap = Heap.create ()
  ; known_vars = Bitset.create ()
  ; in_pool = Bitset.create ()
  ; in_heap = Bitset.create ()
  ; pos_activity = Vec.Value.create ()
  ; neg_activity = Vec.Value.create ()
  ; activity_inc = 1.
  }
;;

let ensure_polarity_capacity t ~var =
  Vec.Value.fill_to_length t.pos_activity ~length:(var + 1) ~f:(fun _ -> 0.);
  Vec.Value.fill_to_length t.neg_activity ~length:(var + 1) ~f:(fun _ -> 0.)
;;

let on_new_var t ~var =
  Heap.ensure_var_capacity t.heap ~var;
  ensure_polarity_capacity t ~var;
  if not (Bitset.get t.known_vars var)
  then (
    Bitset.set t.known_vars var;
    Bitset.set t.in_pool var;
    Bitset.set t.in_heap var;
    Heap.push t.heap var)
;;

let rescale t =
  Heap.rescale_activities t.heap ~scale:1e100;
  Vec.Value.map_inplace t.pos_activity ~f:(fun a -> a /. 1e100);
  Vec.Value.map_inplace t.neg_activity ~f:(fun a -> a /. 1e100);
  t.activity_inc <- t.activity_inc /. 1e100
;;

let add_activity t ~literal =
  let var = Int.abs literal in
  Heap.bump_activity t.heap ~var ~amount:t.activity_inc;
  ensure_polarity_capacity t ~var;
  if literal > 0
  then
    Vec.Value.set
      t.pos_activity
      var
      (Vec.Value.get t.pos_activity var +. t.activity_inc)
  else
    Vec.Value.set
      t.neg_activity
      var
      (Vec.Value.get t.neg_activity var +. t.activity_inc);
  if Bitset.get t.in_heap var then Heap.fix_after_bump t.heap ~var;
  if Float.(Heap.activity t.heap var > 1e100) then rescale t
;;

let decay t = t.activity_inc <- t.activity_inc /. 0.95
let remove_from_pool t ~var = Bitset.clear t.in_pool var

let add_to_pool t ~literal =
  let var = Int.abs literal in
  if not (Bitset.get t.in_pool var) then Bitset.set t.in_pool var;
  if not (Bitset.get t.in_heap var)
  then (
    Bitset.set t.in_heap var;
    Heap.push t.heap var)
;;

let choose_literal t =
  let rec go () =
    match Heap.peek_var t.heap with
    | None -> Null
    | Some var ->
      Heap.remove_at t.heap 0;
      Bitset.clear t.in_heap var;
      if not (Bitset.get t.in_pool var)
      then go ()
      else (
        (* choose the polarity with higher accumulated activity *)
        let pos = Vec.Value.get t.pos_activity var in
        let neg = Vec.Value.get t.neg_activity var in
        This (if Float.compare pos neg >= 0 then var else -var))
  in
  go ()
;;
