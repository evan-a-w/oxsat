open! Core
open Vec_intf

module type S = Vec_intf.S

module Value = struct
  type 'a t =
    { mutable arr : 'a option array
    ; mutable len : int
    }

  let create ?(capacity = 0) () =
    let capacity = Int.max 0 capacity in
    { arr = Array.create ~len:capacity None; len = 0 }
  ;;

  let length t = t.len

  let bounds_check t i =
    if i < 0 || i >= t.len then invalid_arg "Vec index out of bounds"
  ;;

  let get t i =
    bounds_check t i;
    match t.arr.(i) with
    | Some x -> x
    | None -> failwith "Vec internal invariant violated"
  ;;

  let get_opt t i = if i < 0 || i >= t.len then None else t.arr.(i)

  let set t i x =
    bounds_check t i;
    t.arr.(i) <- Some x
  ;;

  let ensure_capacity t needed =
    if needed <= Array.length t.arr
    then ()
    else (
      let rec grow cap = if cap >= needed then cap else grow (Int.max 1 (cap * 2)) in
      let new_cap = grow (Array.length t.arr) in
      let new_arr = Array.create ~len:new_cap None in
      Array.blit ~src:t.arr ~src_pos:0 ~dst:new_arr ~dst_pos:0 ~len:t.len;
      t.arr <- new_arr)
  ;;

  let push t x =
    ensure_capacity t (t.len + 1);
    t.arr.(t.len) <- Some x;
    t.len <- t.len + 1
  ;;

  let pop_exn t =
    if Int.equal t.len 0 then failwith "Vec.pop_exn on empty vec";
    let i = t.len - 1 in
    let x = get t i in
    t.arr.(i) <- None;
    t.len <- i;
    x
  ;;

  let iter t ~f = for i = 0 to t.len - 1 do f (get t i) done
  let iteri t ~f = for i = 0 to t.len - 1 do f i (get t i) done
  let iteri_rev t ~f = for i = t.len - 1 downto 0 do f i (get t i) done
  let iter_rev t ~f = for i = t.len - 1 downto 0 do f (get t i) done

  let iter_nested t ~f = iter t ~f:(fun inner -> iter inner ~f)

  let copy t =
    let out = create ~capacity:t.len () in
    for i = 0 to t.len - 1 do
      push out (get t i)
    done;
    out
  ;;

  let of_array_taking_ownership arr =
    { arr = Array.map arr ~f:(fun x -> Some x); len = Array.length arr }
  ;;

  let fold t ~init ~f =
    let acc = ref init in
    iter t ~f:(fun x -> acc := f !acc x);
    !acc
  ;;

  let foldr t ~init ~f =
    let acc = ref init in
    iter_rev t ~f:(fun x -> acc := f !acc x);
    !acc
  ;;

  let fill_to_length t ~length ~f = while t.len < length do push t (f t.len) done

  let map t ~f =
    let out = create ~capacity:t.len () in
    iter t ~f:(fun x -> push out (f x));
    out
  ;;

  let sort t ~compare =
    let arr = Array.init t.len ~f:(get t) in
    Array.sort arr ~compare;
    t.arr <- Array.map arr ~f:(fun x -> Some x);
    t.len <- Array.length arr
  ;;

  let sort_partitioned t ~a_len:_ ~compare = sort t ~compare

  let fold_map t ~init ~f =
    let acc = ref init in
    let out = create ~capacity:t.len () in
    iter t ~f:(fun x ->
      let acc', y = f !acc x in
      acc := acc';
      push out y);
    out
  ;;

  let of_list xs =
    let out = create ~capacity:(List.length xs) () in
    List.iter xs ~f:(push out);
    out
  ;;

  let to_list t =
    let out = ref [] in
    iter_rev t ~f:(fun x -> out := x :: !out);
    !out
  ;;

  let to_array t = Array.init t.len ~f:(get t)
  let mem t x ~compare =
    try
      iter t ~f:(fun y -> if Int.equal (compare x y) 0 then raise Exit);
      false
    with
    | Exit -> true
  ;;

  let take t ~other =
    t.arr <- Array.copy other.arr;
    t.len <- other.len;
    for i = 0 to other.len - 1 do
      other.arr.(i) <- None
    done;
    other.len <- 0
  ;;

  let switch a b =
    let tmp = { arr = a.arr; len = a.len } in
    a.arr <- b.arr;
    a.len <- b.len;
    b.arr <- tmp.arr;
    b.len <- tmp.len
  ;;

  let last t = if Int.equal t.len 0 then None else Some (get t (t.len - 1))

  let last_exn t =
    match last t with
    | Some x -> x
    | None -> failwith "Vec.last_exn on empty vec"
  ;;

  let filter t ~f =
    let out = create ~capacity:t.len () in
    iter t ~f:(fun x -> if f x then push out x);
    out
  ;;

  let filter_map t ~f =
    let out = create ~capacity:t.len () in
    iter t ~f:(fun x -> Option.iter (f x) ~f:(push out));
    out
  ;;

  let filter_inplace t ~f =
    let write = ref 0 in
    for i = 0 to t.len - 1 do
      let x = get t i in
      if f x
      then (
        t.arr.(!write) <- Some x;
        write := !write + 1)
    done;
    for i = !write to t.len - 1 do
      t.arr.(i) <- None
    done;
    t.len <- !write
  ;;

  let filter_map_inplace t ~f =
    let write = ref 0 in
    for i = 0 to t.len - 1 do
      match f (get t i) with
      | None -> ()
      | Some x ->
        t.arr.(!write) <- Some x;
        write := !write + 1
    done;
    for i = !write to t.len - 1 do
      t.arr.(i) <- None
    done;
    t.len <- !write
  ;;

  let findi t ~f =
    let rec go i =
      if i >= t.len
      then None
      else (
        match f i (get t i) with
        | Some _ as res -> res
        | None -> go (i + 1))
    in
    go 0
  ;;

  let map_inplace t ~f =
    for i = 0 to t.len - 1 do
      t.arr.(i) <- Some (f (get t i))
    done
  ;;

  let singleton x =
    let t = create ~capacity:1 () in
    push t x;
    t
  ;;

  let append t other = iter other ~f:(fun x -> push t x)
  let append_list t xs = List.iter xs ~f:(push t)

  let concat_map t ~f =
    let out = create () in
    iter t ~f:(fun x -> append out (f x));
    out
  ;;

  let concat_mapi t ~f =
    let out = create () in
    iteri t ~f:(fun i x -> append out (f i x));
    out
  ;;

  let concat t = concat_map t ~f:Fn.id
  let concat_list ts = List.fold ts ~init:(create ()) ~f:(fun acc t -> append acc t; acc)

  let to_sequence t = Sequence.of_list (to_list t)

  let clear t =
    for i = 0 to t.len - 1 do
      t.arr.(i) <- None
    done;
    t.len <- 0
  ;;

  let reverse t =
    let out = copy t in
    for i = 0 to (out.len / 2) - 1 do
      let j = out.len - 1 - i in
      let xi = out.arr.(i) in
      out.arr.(i) <- out.arr.(j);
      out.arr.(j) <- xi
    done;
    out
  ;;

  let reverse_inplace t =
    for i = 0 to (t.len / 2) - 1 do
      let j = t.len - 1 - i in
      let xi = t.arr.(i) in
      t.arr.(i) <- t.arr.(j);
      t.arr.(j) <- xi
    done
  ;;

  let zip_exn a b =
    if not (Int.equal a.len b.len) then failwith "Vec.zip_exn: length mismatch";
    let out = create ~capacity:a.len () in
    for i = 0 to a.len - 1 do
      push out (get a i, get b i)
    done;
    out
  ;;

  let binary_search ?end_ t ~f ~which =
    let hi = Option.value end_ ~default:t.len in
    let hi = Int.min hi t.len in
    let lo = 0 in
    let arr = to_array t in
    let rec first_true l r pred =
      if l >= r
      then l
      else (
        let m = (l + r) / 2 in
        if pred arr.(m) then first_true l m pred else first_true (m + 1) r pred)
    in
    let rec last_true l r pred =
      if l >= r
      then l - 1
      else (
        let m = (l + r) / 2 in
        if pred arr.(m) then last_true (m + 1) r pred else last_true l m pred)
    in
    let result_idx =
      match which with
      | `First_ge ->
        let i = first_true lo hi (fun x -> f x >= 0) in
        if i < hi then Some i else None
      | `First_gt ->
        let i = first_true lo hi (fun x -> f x > 0) in
        if i < hi then Some i else None
      | `First_equal ->
        let i = first_true lo hi (fun x -> f x >= 0) in
        if i < hi && Int.equal (f arr.(i)) 0 then Some i else None
      | `Last_lt ->
        let i = last_true lo hi (fun x -> f x < 0) in
        if i >= lo then Some i else None
      | `Last_le ->
        let i = last_true lo hi (fun x -> f x <= 0) in
        if i >= lo then Some i else None
    in
    Option.map result_idx ~f:(Array.get arr)
  ;;
end

module Make (Arg : Elt) = struct
  module Elt = Arg

  type t = Elt.t Value.t

  let create = Value.create
  let length = Value.length
  let get = Value.get
  let set = Value.set
  let iter = Value.iter
  let iteri = Value.iteri
  let iteri_rev = Value.iteri_rev
  let iter_rev = Value.iter_rev
  let fold = Value.fold
  let foldr = Value.foldr
  let push = Value.push
  let pop_exn = Value.pop_exn
  let fill_to_length = Value.fill_to_length
  let take t ~other = Value.take t ~other
  let switch = Value.switch
  let last_exn = Value.last_exn
  let filter = Value.filter
  let filter_inplace = Value.filter_inplace
  let map_inplace = Value.map_inplace
  let singleton = Value.singleton
  let append = Value.append
  let clear = Value.clear
  let reverse_inplace = Value.reverse_inplace
end
