open! Core

module Undo_entry = struct
  type t =
    { child : int
    ; new_root : int
    ; rank_incremented : bool
    }
  [@@deriving sexp_of]
end

type t =
  { parent : int Vec.Value.t
  ; rank : int Vec.Value.t
  ; next : int Vec.Value.t (* circular linked list per class *)
  ; trail : Undo_entry.t Vec.Value.t
  }
[@@deriving sexp_of]

let create ?(capacity = 16) () =
  { parent = Vec.Value.create ~capacity ()
  ; rank = Vec.Value.create ~capacity ()
  ; next = Vec.Value.create ~capacity ()
  ; trail = Vec.Value.create ()
  }
;;

let ensure t i =
  let old_len = Vec.Value.length t.parent in
  for j = old_len to i do
    Vec.Value.push t.parent j;
    Vec.Value.push t.rank 0;
    Vec.Value.push t.next j
  done
;;

let add t =
  let id = Vec.Value.length t.parent in
  ensure t id;
  id
;;

let rec find_root t x =
  let p = Vec.Value.get t.parent x in
  if p = x then x else find_root t p
;;

let find t x =
  ensure t x;
  find_root t x
;;

let splice t a b =
  (* Swap next[a] and next[b], merging or splitting two circular lists. *)
  let a_next = Vec.Value.get t.next a in
  let b_next = Vec.Value.get t.next b in
  Vec.Value.set t.next a b_next;
  Vec.Value.set t.next b a_next
;;

let union t x y =
  ensure t x;
  ensure t y;
  let rx = find_root t x
  and ry = find_root t y in
  if rx = ry
  then Null
  else (
    let rank_rx = Vec.Value.get t.rank rx
    and rank_ry = Vec.Value.get t.rank ry in
    let child, new_root, rank_incremented =
      if rank_rx < rank_ry
      then rx, ry, false
      else if rank_rx > rank_ry
      then ry, rx, false
      else ry, rx, true
    in
    Vec.Value.set t.parent child new_root;
    if rank_incremented
    then Vec.Value.set t.rank new_root (Vec.Value.get t.rank new_root + 1);
    splice t child new_root;
    This { Undo_entry.child; new_root; rank_incremented })
;;

let same_class t x y =
  ensure t x;
  ensure t y;
  find_root t x = find_root t y
;;

let size t = Vec.Value.length t.parent

let undo t ~undo_entry:{ Undo_entry.child; new_root; rank_incremented } =
  splice t child new_root;
  Vec.Value.set t.parent child child;
  if rank_incremented
  then Vec.Value.set t.rank new_root (Vec.Value.get t.rank new_root - 1)
;;

let iter_class t x ~f =
  ensure t x;
  f x;
  let cur = ref (Vec.Value.get t.next x) in
  while !cur <> x do
    f !cur;
    cur := Vec.Value.get t.next !cur
  done
;;
