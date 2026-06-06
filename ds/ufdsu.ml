open! Core

module Undo_entry = struct
  type t =
    { child : int
    ; new_root : int
    ; rank_incremented : bool
    }
  [@@deriving sexp_of]
end

module Level = struct
  type t = int [@@deriving sexp_of]
end

type t =
  { parent : int Vec.Value.t
  ; rank : int Vec.Value.t
  ; trail : Undo_entry.t Vec.Value.t
  }
[@@deriving sexp_of]

let create ?(capacity = 16) () =
  { parent = Vec.Value.create ~capacity ()
  ; rank = Vec.Value.create ~capacity ()
  ; trail = Vec.Value.create ()
  }
;;

let ensure t i =
  let old_len = Vec.Value.length t.parent in
  for j = old_len to i do
    Vec.Value.push t.parent j;
    Vec.Value.push t.rank 0
  done
;;

let add t =
  let id = Vec.Value.length t.parent in
  ensure t id;
  id
;;

(* No path compression — required for undo correctness. *)
let rec find_root t x =
  let p = Vec.Value.get t.parent x in
  if p = x then x else find_root t p
;;

let find t x =
  ensure t x;
  find_root t x
;;

let union t x y =
  ensure t x;
  ensure t y;
  let rx = find_root t x
  and ry = find_root t y in
  if rx = ry
  then false
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
    Vec.Value.push t.trail { Undo_entry.child; new_root; rank_incremented };
    true)
;;

let same_class t x y =
  ensure t x;
  ensure t y;
  find_root t x = find_root t y
;;

let size t = Vec.Value.length t.parent
let save t : Level.t = Vec.Value.length t.trail

let restore t (level : Level.t) =
  while Vec.Value.length t.trail > level do
    let { Undo_entry.child; new_root; rank_incremented } =
      Vec.Value.pop_exn t.trail
    in
    Vec.Value.set t.parent child child;
    if rank_incremented
    then Vec.Value.set t.rank new_root (Vec.Value.get t.rank new_root - 1)
  done
;;
