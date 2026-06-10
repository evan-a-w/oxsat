open! Core
open! Ds

let%expect_test "basic union and same_class" =
  let t = Ufds.create () in
  let a = Ufds.add t in
  let b = Ufds.add t in
  let c = Ufds.add t in
  print_s
    [%message
      "before union"
        ~ab:(Ufds.same_class t a b : bool)
        ~bc:(Ufds.same_class t b c : bool)
        ~ac:(Ufds.same_class t a c : bool)];
  let merged = Ufds.union t a b in
  print_s [%message "union a b" ~merged:(merged : bool)];
  print_s
    [%message
      "after union a-b"
        ~ab:(Ufds.same_class t a b : bool)
        ~bc:(Ufds.same_class t b c : bool)
        ~ac:(Ufds.same_class t a c : bool)];
  let merged2 = Ufds.union t b c in
  print_s [%message "union b c" ~merged:(merged2 : bool)];
  print_s
    [%message
      "after union b-c"
        ~ab:(Ufds.same_class t a b : bool)
        ~bc:(Ufds.same_class t b c : bool)
        ~ac:(Ufds.same_class t a c : bool)];
  [%expect
    {|
    ("before union" (ab false) (bc false) (ac false))
    ("union a b" (merged true))
    ("after union a-b" (ab true) (bc false) (ac false))
    ("union b c" (merged true))
    ("after union b-c" (ab true) (bc true) (ac true))
    |}]
;;

let%expect_test "union returns false when already same class" =
  let t = Ufds.create () in
  let a = Ufds.add t in
  let b = Ufds.add t in
  ignore (Ufds.union t a b : bool);
  let redundant = Ufds.union t a b in
  print_s [%message (redundant : bool)];
  [%expect {| (redundant false) |}]
;;

let%expect_test "self same_class" =
  let t = Ufds.create () in
  let a = Ufds.add t in
  print_s [%message (Ufds.same_class t a a : bool)];
  [%expect {| ("Ufds.same_class t a a" true) |}]
;;

let%expect_test "find is idempotent" =
  let t = Ufds.create () in
  let a = Ufds.add t in
  let b = Ufds.add t in
  let c = Ufds.add t in
  ignore (Ufds.union t a b : bool);
  ignore (Ufds.union t b c : bool);
  let ra = Ufds.find t a in
  print_s [%message (ra = Ufds.find t ra : bool)];
  [%expect {| ("ra = (Ufds.find t ra)" true) |}]
;;

let%expect_test "auto-resize via find and union" =
  let t = Ufds.create ~capacity:4 () in
  (* access elements well beyond initial capacity without calling add *)
  ignore (Ufds.union t 0 99 : bool);
  print_s [%message (Ufds.same_class t 0 99 : bool) (Ufds.size t : int)];
  [%expect {| (("Ufds.same_class t 0 99" true) ("Ufds.size t" 100)) |}]
;;

let%expect_test "size tracks elements" =
  let t = Ufds.create () in
  print_s [%message (Ufds.size t : int)];
  ignore (Ufds.add t : int);
  ignore (Ufds.add t : int);
  ignore (Ufds.add t : int);
  print_s [%message (Ufds.size t : int)];
  [%expect {|
    ("Ufds.size t" 0)
    ("Ufds.size t" 3)
    |}]
;;

(* Quickcheck: compare against a naive reference union-find *)
module Reference = struct
  type t = int Int.Table.t

  let create () : t = Int.Table.create ()

  let ensure (t : t) i =
    for j = Hashtbl.length t to i do
      Hashtbl.set t ~key:j ~data:j
    done
  ;;

  let rec find (t : t) x =
    let p = Hashtbl.find_exn t x in
    if p = x then x else find t p
  ;;

  let union (t : t) x y =
    ensure t x;
    ensure t y;
    let rx = find t x
    and ry = find t y in
    if rx = ry
    then false
    else (
      Hashtbl.set t ~key:rx ~data:ry;
      true)
  ;;

  let same_class (t : t) x y =
    ensure t x;
    ensure t y;
    find t x = find t y
  ;;
end

type operation =
  | Union of int * int
  | Same_class of int * int
[@@deriving sexp_of]

let quickcheck_generator_operation =
  let open Quickcheck.Generator.Let_syntax in
  let gen_elem = Int.gen_incl 0 15 in
  Quickcheck.Generator.union
    [ (let%map x = gen_elem
       and y = gen_elem in
       Union (x, y))
    ; (let%map x = gen_elem
       and y = gen_elem in
       Same_class (x, y))
    ]
;;

let%test_unit "quickcheck matches reference" =
  Quickcheck.test
    ~trials:500
    ~sexp_of:[%sexp_of: operation list]
    (Quickcheck.Generator.list quickcheck_generator_operation)
    ~f:(fun ops ->
      let t = Ufds.create () in
      let ref_ = Reference.create () in
      List.iter ops ~f:(fun op ->
        match op with
        | Union (x, y) ->
          let ours = Ufds.union t x y in
          let theirs = Reference.union ref_ x y in
          (* both should agree on whether it was a new merge *)
          if Bool.( <> ) ours theirs
          then
            failwith (sprintf "union(%d,%d): ours=%b theirs=%b" x y ours theirs)
        | Same_class (x, y) ->
          let ours = Ufds.same_class t x y in
          let theirs = Reference.same_class ref_ x y in
          if Bool.( <> ) ours theirs
          then
            failwith
              (sprintf "same_class(%d,%d): ours=%b theirs=%b" x y ours theirs)))
;;
