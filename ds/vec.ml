open! Core
open Vec_intf

module Value = struct
  type 'a t =
    { mutable arr : 'a option array
    ; mutable len : int
    }

  let create ?(capacity = 0) () =
    { arr = Array.create ~len:(Int.max 0 capacity) None; len = 0 }
  ;;

  let length t = t.len

  let ensure_capacity t needed =
    if needed <= Array.length t.arr
    then ()
    else (
      let rec grow n = if n >= needed then n else grow (Int.max 1 (2 * n)) in
      let new_len = grow (Array.length t.arr) in
      let arr = Array.create ~len:new_len None in
      Array.blit ~src:t.arr ~src_pos:0 ~dst:arr ~dst_pos:0 ~len:t.len;
      t.arr <- arr)
  ;;

  let get t i =
    if i < 0 || i >= t.len then invalid_arg "Vec.get";
    match t.arr.(i) with
    | Some x -> x
    | None -> failwith "Vec.get: invariant"
  ;;

  let set t i x =
    if i < 0 || i >= t.len then invalid_arg "Vec.set";
    t.arr.(i) <- Some x
  ;;

  let iteri t ~f = for i = 0 to t.len - 1 do f i (get t i) done
  let iter_rev t ~f = for i = t.len - 1 downto 0 do f (get t i) done

  let push t x =
    ensure_capacity t (t.len + 1);
    t.arr.(t.len) <- Some x;
    t.len <- t.len + 1
  ;;

  let pop_exn t =
    if t.len = 0 then failwith "Vec.pop_exn";
    let i = t.len - 1 in
    let x = get t i in
    t.arr.(i) <- None;
    t.len <- i;
    x
  ;;

  let fill_to_length t ~length ~f = while t.len < length do push t (f t.len) done

  let clear t =
    for i = 0 to t.len - 1 do
      t.arr.(i) <- None
    done;
    t.len <- 0
  ;;

  let last_exn t = if t.len = 0 then failwith "Vec.last_exn" else get t (t.len - 1)

  let sort t ~compare =
    let a = Array.init t.len ~f:(get t) in
    Array.sort a ~compare;
    clear t;
    Array.iter a ~f:(push t)
  ;;
end

module Make (Arg : Elt) = struct
  module Elt = Arg

  type t = Elt.t Value.t

  let create = Value.create
  let length = Value.length
  let get = Value.get
  let set = Value.set
  let iter_rev = Value.iter_rev
  let last_exn = Value.last_exn
  let push = Value.push
  let pop_exn = Value.pop_exn
end
