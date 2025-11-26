open! Core
open! Unboxed
open Vec_intf

module type%template
  [@kind
    k
    = ( value
      , float64
      , value & value
      , value & value & value
      , bits64
      , bits64 & bits64
      , immediate & value & value
      , bits64 & bits64 & bits64
      , bits64 & bits64 & immediate & immediate & bits64
      , bits64 & bits64 & value & value & bits64
      , (value & value & bits64) & bits64 & bits64 )] S = S [@kind k]

module%template
  [@kind
    k
    = ( value
      , value & value
      , bits64
      , float64
      , immediate & value & value
      , bits64 & bits64
      , value & value & value
      , bits64 & bits64 & bits64
      , bits64 & bits64 & immediate & immediate & bits64
      , bits64 & bits64 & value & value & bits64
      , (value & value & bits64) & bits64 & bits64 )] Make
    (Arg : Elt
  [@kind k]) : S [@kind k] with module Elt = Arg = struct
  module Elt = Arg

  type t =
    { mutable arr : Elt.t array
    ; mutable length : int
    }

  let create ?(capacity = 0) () =
    { arr = Array.create ~len:capacity (Elt.create_for_vec ()); length = 0 }
  ;;

  let clear t =
    t.arr <- [||];
    t.length <- 0
  ;;

  let singleton x = { arr = [| x |]; length = 1 }
  let length t = t.length

  let rec push t v =
    if t.length = Array.length t.arr
    then (
      let new_len = 2 * (t.length + 1) in
      let new_ = Array.create ~len:new_len (Elt.create_for_vec ()) in
      for i = 0 to t.length - 1 do
        new_.(i) <- t.arr.(i)
      done;
      t.arr <- new_;
      push t v)
    else (
      t.arr.(t.length) <- v;
      t.length <- t.length + 1)
  ;;

  let pop_exn t : Elt.t =
    if t.length = 0 then raise (Invalid_argument "Empty array");
    t.length <- t.length - 1;
    t.arr.(t.length)
  ;;

  let get t i : Elt.t =
    if i < 0 || i >= t.length
    then raise (Invalid_argument [%string "Index %{i#Int} out of bounds"]);
    t.arr.(i)
  ;;

  let set t i v =
    if i < 0 || i >= t.length
    then raise (Invalid_argument "Index out of bounds");
    t.arr.(i) <- v
  ;;

  let iter t ~f =
    for i = 0 to t.length - 1 do
      f t.arr.(i)
    done
  ;;

  let iteri t ~f =
    for i = 0 to t.length - 1 do
      f i t.arr.(i)
    done
  ;;

  let iteri_rev t ~(local_ f) =
    for i = t.length - 1 downto 0 do
      f i t.arr.(i)
    done
  ;;

  let iter_rev t ~(local_ f) = iteri_rev t ~f:(fun _ x -> f x) [@nontail]

  let fold t ~init ~f =
    let r = ref init in
    for i = 0 to t.length - 1 do
      r := f !r t.arr.(i)
    done;
    !r
  ;;

  let foldr t ~init ~f =
    let r = ref init in
    for i = length t - 1 downto 0 do
      r := f !r t.arr.(i)
    done;
    !r
  ;;

  let fill_to_length t ~length ~f =
    let i = ref (t.length - 1) in
    while !i < length - 1 do
      push t (f !i);
      incr i
    done
  ;;

  let take t ~other =
    t.arr <- other.arr;
    t.length <- other.length;
    other.arr <- [||];
    other.length <- 0
  ;;

  let switch t1 t2 =
    let arr = t1.arr in
    let length = t1.length in
    t1.arr <- t2.arr;
    t1.length <- t2.length;
    t2.arr <- arr;
    t2.length <- length
  ;;

  let last_exn t = get t (length t - 1)

  let filter t ~f =
    let new_ = create () in
    for i = 0 to t.length - 1 do
      if f t.arr.(i) then push new_ t.arr.(i)
    done;
    new_
  ;;

  let map_inplace t ~f =
    for i = 0 to t.length - 1 do
      t.arr.(i) <- f t.arr.(i)
    done
  ;;

  let filter_inplace t ~f =
    let write = ref 0 in
    for i = 0 to t.length - 1 do
      if f t.arr.(i)
      then (
        t.arr.(!write) <- t.arr.(i);
        incr write)
    done;
    t.length <- !write
  ;;

  let append t t' = iter t' ~f:(push t)

  let reverse_inplace t =
    let end_ = ref (length t - 1) in
    let start = ref 0 in
    while !start < !end_ do
      let tmp = t.arr.(!start) in
      t.arr.(!start) <- t.arr.(!end_);
      t.arr.(!end_) <- tmp;
      incr start;
      decr end_
    done
  ;;
end

module Value = struct
  type 'a t =
    { mutable arr : 'a array
    ; mutable length : int
    }
  [@@deriving fields]

  let create ?(capacity = 0) () =
    { arr = Array.create ~len:capacity (Obj.magic ()); length = 0 }
  ;;

  let clear t =
    t.arr <- [||];
    t.length <- 0
  ;;

  let singleton x = { arr = [| x |]; length = 1 }
  let length t = t.length

  let rec push t v =
    if t.length = Array.length t.arr
    then (
      let new_len = 2 * (t.length + 1) in
      let new_ = Array.create ~len:new_len (Obj.magic ()) in
      Array.blit ~src:t.arr ~src_pos:0 ~dst:new_ ~dst_pos:0 ~len:t.length;
      t.arr <- new_;
      push t v)
    else (
      t.arr.(t.length) <- v;
      t.length <- t.length + 1)
  ;;

  let pop_exn t =
    if t.length = 0
    then raise (Invalid_argument "Empty array")
    else (
      t.length <- t.length - 1;
      t.arr.(t.length))
  ;;

  let get t i =
    if i < 0 || i >= t.length
    then raise (Invalid_argument [%string "Index %{i#Int} out of bounds"])
    else t.arr.(i)
  ;;

  let get_opt t i = if i < 0 || i >= t.length then None else Some t.arr.(i)

  let set t i v =
    if i < 0 || i >= t.length
    then raise (Invalid_argument "Index out of bounds")
    else t.arr.(i) <- v
  ;;

  let iter t ~f =
    for i = 0 to t.length - 1 do
      f t.arr.(i)
    done
  ;;

  let iteri t ~f =
    for i = 0 to t.length - 1 do
      f i t.arr.(i)
    done
  ;;

  let iteri_rev t ~f =
    for i = t.length - 1 downto 0 do
      f i t.arr.(i)
    done
  ;;

  let%template fold t ~init ~f =
    let r = ref init in
    for i = 0 to t.length - 1 do
      r := f !r t.arr.(i)
    done;
    !r
  [@@mode global]
  ;;

  let%template fold t ~(local_ init) ~f = exclave_
    let rec go acc i = exclave_
      if i >= length t then acc else go (f acc (get t i)) (i + 1)
    in
    go init 0
  [@@mode local]
  ;;

  let foldr t ~init ~f =
    let r = ref init in
    for i = length t - 1 downto 0 do
      r := f !r t.arr.(i)
    done;
    !r
  ;;

  let copy t =
    { arr = Array.sub t.arr ~pos:0 ~len:(length t); length = length t }
  ;;

  let fill_to_length t ~length ~f =
    let i = ref (t.length - 1) in
    while !i < length - 1 do
      push t (f !i);
      incr i
    done
  ;;

  let map t ~f =
    let new_ = create ~capacity:t.length () in
    for i = 0 to t.length - 1 do
      push new_ (f t.arr.(i))
    done;
    new_
  ;;

  let fold_map t ~init ~f =
    let acc = ref init in
    let new_ = create ~capacity:t.length () in
    for i = 0 to t.length - 1 do
      let acc', x = f !acc t.arr.(i) in
      acc := acc';
      push new_ x
    done;
    new_
  ;;

  let zip_exn a b =
    let new_ = create ~capacity:a.length () in
    if a.length <> b.length then failwith "[zip_exn] diff lengths";
    for i = 0 to a.length - 1 do
      push new_ (a.arr.(i), b.arr.(i))
    done;
    new_
  ;;

  let of_list l =
    let t = create ~capacity:(List.length l) () in
    List.iter l ~f:(push t);
    t
  ;;

  let to_list t = fold t ~init:[] ~f:(fun acc x -> x :: acc) |> List.rev
  let to_array t = Array.sub t.arr ~pos:0 ~len:t.length

  let%expect_test "push" =
    let t = create () in
    push t 1;
    push t 2;
    push t 3;
    push t 4;
    push t 5;
    iter t ~f:(fun x -> Int.to_string x |> print_endline);
    [%expect {|
    1
    2
    3
    4
    5
    |}]
  ;;

  let sexp_of_t sexp_of_a t = [%sexp_of: a list] (to_list t)
  let t_of_sexp a_of_sexp sexp = [%of_sexp: a list] sexp |> of_list

  let mem t v ~compare =
    let rec loop i =
      if i = t.length
      then false
      else if compare t.arr.(i) v = 0
      then true
      else loop (i + 1)
    in
    loop 0
  ;;

  let take t ~other =
    t.arr <- other.arr;
    t.length <- other.length;
    other.arr <- [||];
    other.length <- 0
  ;;

  let switch t1 t2 =
    let arr = t1.arr in
    let length = t1.length in
    t1.arr <- t2.arr;
    t1.length <- t2.length;
    t2.arr <- arr;
    t2.length <- length
  ;;

  let last_exn t = get t (length t - 1)
  let last t = if length t = 0 then None else Some (get t (length t - 1))

  let filter t ~f =
    let new_ = create () in
    for i = 0 to t.length - 1 do
      if f t.arr.(i) then push new_ t.arr.(i)
    done;
    new_
  ;;

  let filter_map t ~f =
    let new_ = create () in
    for i = 0 to t.length - 1 do
      match f t.arr.(i) with
      | Some x -> push new_ x
      | None -> ()
    done;
    new_
  ;;

  let findi t ~f =
    let res = ref None in
    let i = ref 0 in
    while !i < t.length && Option.is_none !res do
      (match f !i t.arr.(!i) with
       | Some _ as x -> res := x
       | None -> ());
      incr i
    done;
    !res
  ;;

  let map_inplace t ~f =
    for i = 0 to t.length - 1 do
      t.arr.(i) <- f t.arr.(i)
    done
  ;;

  let filter_inplace t ~f =
    let write = ref 0 in
    for i = 0 to t.length - 1 do
      if f t.arr.(i)
      then (
        t.arr.(!write) <- t.arr.(i);
        incr write)
    done;
    t.length <- !write
  ;;

  let filter_map_inplace t ~f =
    let write = ref 0 in
    for i = 0 to t.length - 1 do
      match f t.arr.(i) with
      | None -> ()
      | Some x ->
        t.arr.(!write) <- x;
        incr write
    done;
    t.length <- !write
  ;;

  let append t t' = iter t' ~f:(push t)
  let append_list t l = List.iter l ~f:(push t)

  let concat_mapi t ~f =
    let new_ = create () in
    for i = 0 to t.length do
      f i t.arr.(i) |> append new_
    done;
    new_
  ;;

  let concat_map t ~f =
    let new_ = create () in
    for i = 0 to t.length - 1 do
      f t.arr.(i) |> append new_
    done;
    new_
  ;;

  let concat t =
    let new_ = create () in
    iter t ~f:(iter ~f:(push new_));
    new_
  ;;

  let concat_list l =
    let new_ = create () in
    List.iter l ~f:(iter ~f:(push new_));
    new_
  ;;

  let to_sequence t =
    Sequence.unfold ~init:0 ~f:(fun i ->
      get_opt t i |> Option.map ~f:(fun x -> x, i + 1))
  ;;

  let iter_nested t ~f = iter t ~f:(iter ~f) [@nontail]
  let iter_rev t ~f = iteri_rev t ~f:(fun _ x -> f x) [@nontail]

  let reverse t =
    let new_ = create ~capacity:(length t) () in
    iter_rev t ~f:(push new_);
    new_
  ;;

  let sort t ~compare = Array.sort t.arr ~len:t.length ~compare

  let reverse_inplace t =
    let end_ = ref (length t - 1) in
    let start = ref 0 in
    while !start < !end_ do
      let tmp = t.arr.(!start) in
      t.arr.(!start) <- t.arr.(!end_);
      t.arr.(!end_) <- tmp;
      incr start;
      decr end_
    done
  ;;

  let%expect_test "concat_map" =
    let a = of_list [ 1; 2; 3; 4; 5 ] in
    print_s
      [%sexp
        (concat_map a ~f:(fun i -> List.init i ~f:Fn.id |> of_list) : int t)];
    [%expect {| (0 0 1 0 1 2 0 1 2 3 0 1 2 3 4) |}]
  ;;

  let sort_partitioned t ~a_len ~(local_ compare) =
    let b_len = length t - a_len in
    if length t = 0 || b_len <= 0
    then ()
    else (
      let b = Array.create__stack ~len:b_len (get t 0) in
      Array.blit ~src:t.arr ~src_pos:a_len ~len:b_len ~dst:b ~dst_pos:0;
      let rec go i a_i b_i =
        if i < 0
        then ()
        else if b_i < 0
        then (
          t.arr.(i) <- t.arr.(a_i);
          go (i - 1) (a_i - 1) b_i)
        else if a_i < 0
        then (
          t.arr.(i) <- b.(b_i);
          go (i - 1) a_i (b_i - 1))
        else if compare t.arr.(a_i) b.(b_i) < 0
        then (
          t.arr.(i) <- b.(b_i);
          go (i - 1) a_i (b_i - 1))
        else (
          t.arr.(i) <- t.arr.(a_i);
          go (i - 1) (a_i - 1) b_i)
      in
      go (length t - 1) (a_len - 1) (b_len - 1) [@nontail])
  ;;

  let%expect_test "sort_partitioned" =
    let t = of_list [ 1; 3; 5; 7; 2; 4; 6; 8 ] in
    sort_partitioned t ~a_len:4 ~compare:Int.compare;
    print_s [%sexp (to_list t : int list)];
    [%expect {| (1 2 3 4 5 6 7 8) |}];
    let t2 = of_list [ 1; 2; 3; 4; 5 ] in
    sort_partitioned t2 ~a_len:2 ~compare:Int.compare;
    print_s [%sexp (to_list t2 : int list)];
    [%expect {| (1 2 3 4 5) |}]
  ;;

  let binary_search ?(local_ end_) t ~f ~which = exclave_
    let len =
      match end_ with
      | None -> t.length
      | Some len -> len
    in
    if len = 0
    then None
    else (
      let accept, search_left =
        match which with
        | `First_equal -> (fun c -> c = 0), true
        | `First_ge -> (fun c -> c >= 0), true
        | `First_gt -> (fun c -> c > 0), true
        | `Last_le -> (fun c -> c <= 0), false
        | `Last_lt -> (fun c -> c < 0), false
      in
      let rec loop left right best = exclave_
        if left > right
        then best
        else (
          let mid = left + ((right - left) / 2) in
          let c = f t.arr.(mid) in
          if accept c
          then
            if search_left
            then loop left (mid - 1) (Some t.arr.(mid))
            else loop (mid + 1) right (Some t.arr.(mid))
          else if c < 0
          then loop (mid + 1) right best
          else if c > 0
          then loop left (mid - 1) best
          else if search_left
          then loop (mid + 1) right best
          else loop left (mid - 1) best)
      in
      loop 0 (len - 1) None)
  ;;

  let%expect_test "binary_search" =
    let t = of_list [ 1; 3; 3; 3; 5; 7; 9 ] in
    let search target which =
      [%globalize: int option]
        (binary_search t ~f:(fun x -> Int.compare x target) ~which) [@nontail]
    in
    print_s [%sexp (search 3 `First_equal : int option)];
    [%expect {| (3) |}];
    print_s [%sexp (search 3 `First_ge : int option)];
    [%expect {| (3) |}];
    print_s [%sexp (search 3 `First_gt : int option)];
    [%expect {| (5) |}];
    print_s [%sexp (search 3 `Last_le : int option)];
    [%expect {| (3) |}];
    print_s [%sexp (search 3 `Last_lt : int option)];
    [%expect {| (1) |}];
    print_s [%sexp (search 4 `First_ge : int option)];
    [%expect {| (5) |}];
    print_s [%sexp (search 10 `First_ge : int option)];
    [%expect {| () |}];
    print_s [%sexp (search 0 `Last_le : int option)];
    [%expect {| () |}]
  ;;
end
