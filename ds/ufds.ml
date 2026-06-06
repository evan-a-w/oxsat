open! Core

type t =
  { parent : int Vec.Value.t
  ; rank : int Vec.Value.t
  }

let sexp_of_t t =
  [%sexp_of: int Vec.Value.t * int Vec.Value.t] (t.parent, t.rank)
;;

let create ?(capacity = 16) () =
  { parent = Vec.Value.create ~capacity (); rank = Vec.Value.create ~capacity () }
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
  Vec.Value.push t.parent id;
  Vec.Value.push t.rank 0;
  id
;;

let rec find_root t x =
  let p = Vec.Value.get t.parent x in
  if p = x
  then x
  else (
    let root = find_root t p in
    Vec.Value.set t.parent x root;
    root)
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
    (if rank_rx < rank_ry
     then Vec.Value.set t.parent rx ry
     else if rank_rx > rank_ry
     then Vec.Value.set t.parent ry rx
     else (
       Vec.Value.set t.parent ry rx;
       Vec.Value.set t.rank rx (rank_rx + 1)));
    true)
;;

let same_class t x y =
  ensure t x;
  ensure t y;
  find_root t x = find_root t y
;;

let size t = Vec.Value.length t.parent
