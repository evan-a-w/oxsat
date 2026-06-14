open! Core

type 'a t =
  { data : 'a Vec.Value.t
  ; compare : 'a -> 'a -> int
  }

let create ?(capacity = 0) ~compare () =
  { data = Vec.Value.create ~capacity (); compare }
;;

let length t = Vec.Value.length t.data
let is_empty t = length t = 0
let clear t = Vec.Value.clear t.data

let parent i = (i - 1) / 2
let left i = (2 * i) + 1
let right i = (2 * i) + 2

let sift_up t i =
  let rec go i =
    if i > 0
    then (
      let p = parent i in
      if t.compare (Vec.Value.get t.data i) (Vec.Value.get t.data p) < 0
      then (
        Vec.Value.swap t.data i p;
        go p))
  in
  go i
;;

let sift_down t i =
  let n = length t in
  let rec go i =
    let l = left i in
    let r = right i in
    let smallest =
      if l < n
         && t.compare (Vec.Value.get t.data l) (Vec.Value.get t.data i) < 0
      then l
      else i
    in
    let smallest =
      if r < n
         && t.compare (Vec.Value.get t.data r) (Vec.Value.get t.data smallest)
            < 0
      then r
      else smallest
    in
    if smallest <> i
    then (
      Vec.Value.swap t.data i smallest;
      go smallest)
  in
  go i
;;

let push t v =
  Vec.Value.push t.data v;
  sift_up t (length t - 1)
;;

let peek_exn t = Vec.Value.get t.data 0
let peek t = if is_empty t then None else Some (peek_exn t)

let pop_exn t =
  let top = peek_exn t in
  let last = Vec.Value.pop_exn t.data in
  if not (is_empty t)
  then (
    Vec.Value.set t.data 0 last;
    sift_down t 0);
  top
;;

let pop t = if is_empty t then None else Some (pop_exn t)

let of_list ~compare l =
  let t = create ~compare ~capacity:(List.length l) () in
  List.iter l ~f:(push t);
  t
;;

let to_list t = Vec.Value.to_list t.data
let sexp_of_t sexp_of_a t = [%sexp_of: a list] (to_list t)

let%expect_test "push and pop in sorted order" =
  let t = create ~compare:Int.compare () in
  List.iter [ 5; 3; 8; 1; 9; 2; 7; 4; 6; 0 ] ~f:(push t);
  print_s [%sexp (t : int t)];
  [%expect {| (0 1 2 5 4 3 7 8 6 9) |}];
  while not (is_empty t) do
    pop_exn t |> Int.to_string |> print_endline
  done;
  [%expect {|
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    |}]
;;

let%expect_test "max-heap via custom compare" =
  let t = of_list ~compare:(fun a b -> Int.compare b a) [ 5; 3; 8; 1; 9 ] in
  while not (is_empty t) do
    pop_exn t |> Int.to_string |> print_endline
  done;
  [%expect {|
    9
    8
    5
    3
    1
    |}]
;;

let%expect_test "peek does not remove" =
  let t = of_list ~compare:Int.compare [ 3; 1; 2 ] in
  print_s [%sexp (peek t : int option)];
  [%expect {| (1) |}];
  print_s [%sexp (length t : int)];
  [%expect {| 3 |}]
;;

let%expect_test "empty heap" =
  let t = create ~compare:Int.compare () in
  print_s [%sexp (peek t : int option)];
  [%expect {| () |}];
  print_s [%sexp (pop t : int option)];
  [%expect {| () |}]
;;
