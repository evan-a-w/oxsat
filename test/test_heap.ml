open! Core
open! Ds

let%expect_test "basic push/pop order" =
  let t = Heap.create ~compare:Int.compare () in
  List.iter [ 5; 3; 8; 1; 9; 2 ] ~f:(Heap.push t);
  print_s [%message (Heap.length t : int)];
  let popped = ref [] in
  while not (Heap.is_empty t) do
    popped := Heap.pop_exn t :: !popped
  done;
  print_s [%message (List.rev !popped : int list)];
  [%expect
    {|
    ("Heap.length t" 6)
    ("List.rev (!popped)" (1 2 3 5 8 9))
    |}]
;;

let%expect_test "of_list and peek" =
  let t = Heap.of_list ~compare:Int.compare [ 4; 2; 7; 1; 3 ] in
  print_s [%message (Heap.peek t : int option) (Heap.length t : int)];
  ignore (Heap.pop_exn t : int);
  print_s [%message (Heap.peek t : int option) (Heap.length t : int)];
  [%expect
    {|
    (("Heap.peek t" (1)) ("Heap.length t" 5))
    (("Heap.peek t" (2)) ("Heap.length t" 4))
    |}]
;;

let%expect_test "max-heap via reversed compare" =
  let t =
    Heap.of_list ~compare:(fun a b -> Int.compare b a) [ 5; 3; 8; 1; 9 ]
  in
  let popped = ref [] in
  while not (Heap.is_empty t) do
    popped := Heap.pop_exn t :: !popped
  done;
  print_s [%sexp (List.rev !popped : int list)];
  [%expect {| (9 8 5 3 1) |}]
;;

let%expect_test "empty heap" =
  let t = Heap.create ~compare:Int.compare () in
  print_s [%message (Heap.peek t : int option) (Heap.pop t : int option)];
  [%expect {| (("Heap.peek t" ()) ("Heap.pop t" ())) |}]
;;

let%expect_test "clear" =
  let t = Heap.of_list ~compare:Int.compare [ 3; 1; 2 ] in
  Heap.clear t;
  print_s [%message (Heap.is_empty t : bool) (Heap.length t : int)];
  [%expect {| (("Heap.is_empty t" true) ("Heap.length t" 0)) |}]
;;

(* Quickcheck: interleaved pushes/pops should match a sorted-list reference. *)
type operation =
  | Push of int
  | Pop
[@@deriving sexp_of]

let quickcheck_generator_operation =
  let open Quickcheck.Generator.Let_syntax in
  Quickcheck.Generator.union
    [ (let%map x = Int.gen_incl (-100) 100 in
       Push x)
    ; Quickcheck.Generator.return Pop
    ]
;;

let%test_unit "quickcheck matches sorted-list reference" =
  Quickcheck.test
    ~trials:500
    ~sexp_of:[%sexp_of: operation list]
    (Quickcheck.Generator.list quickcheck_generator_operation)
    ~f:(fun ops ->
      let t = Heap.create ~compare:Int.compare () in
      let reference = ref [] in
      List.iter ops ~f:(fun op ->
        match op with
        | Push x ->
          Heap.push t x;
          reference := List.sort (x :: !reference) ~compare:Int.compare
        | Pop ->
          let ours = Heap.pop t in
          let theirs =
            match !reference with
            | [] -> None
            | x :: rest ->
              reference := rest;
              Some x
          in
          if [%compare: int option] ours theirs <> 0
          then
            failwith
              (sprintf
                 "pop: ours=%s theirs=%s"
                 (Sexp.to_string [%sexp (ours : int option)])
                 (Sexp.to_string [%sexp (theirs : int option)])))
      [@nontail])
;;
