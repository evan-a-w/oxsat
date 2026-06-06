open! Core
open! Ds

let%expect_test "basic save and restore" =
  let t = Ufdsu.create () in
  let a = Ufdsu.add t in
  let b = Ufdsu.add t in
  let c = Ufdsu.add t in
  ignore (Ufdsu.union t a b : bool);
  let level = Ufdsu.save t in
  ignore (Ufdsu.union t b c : bool);
  print_s
    [%message
      "before restore"
        ~ab:(Ufdsu.same_class t a b : bool)
        ~bc:(Ufdsu.same_class t b c : bool)
        ~ac:(Ufdsu.same_class t a c : bool)];
  Ufdsu.restore t level;
  print_s
    [%message
      "after restore"
        ~ab:(Ufdsu.same_class t a b : bool)
        ~bc:(Ufdsu.same_class t b c : bool)
        ~ac:(Ufdsu.same_class t a c : bool)];
  [%expect
    {|
    ("before restore" (ab true) (bc true) (ac true))
    ("after restore" (ab true) (bc false) (ac false))
    |}]
;;

let%expect_test "nested save and restore" =
  let t = Ufdsu.create () in
  let a = Ufdsu.add t in
  let b = Ufdsu.add t in
  let c = Ufdsu.add t in
  let d = Ufdsu.add t in
  let l0 = Ufdsu.save t in
  ignore (Ufdsu.union t a b : bool);
  let l1 = Ufdsu.save t in
  ignore (Ufdsu.union t c d : bool);
  ignore (Ufdsu.union t a c : bool);
  print_s [%message "all merged" ~abcd:(Ufdsu.same_class t a d : bool)];
  Ufdsu.restore t l1;
  print_s
    [%message
      "after restore to l1"
        ~ab:(Ufdsu.same_class t a b : bool)
        ~cd:(Ufdsu.same_class t c d : bool)
        ~ac:(Ufdsu.same_class t a c : bool)];
  Ufdsu.restore t l0;
  print_s
    [%message
      "after restore to l0"
        ~ab:(Ufdsu.same_class t a b : bool)
        ~cd:(Ufdsu.same_class t c d : bool)];
  [%expect
    {|
    ("all merged" (abcd true))
    ("after restore to l1" (ab true) (cd false) (ac false))
    ("after restore to l0" (ab false) (cd false))
    |}]
;;

let%expect_test "union after restore returns true again" =
  let t = Ufdsu.create () in
  let a = Ufdsu.add t in
  let b = Ufdsu.add t in
  let level = Ufdsu.save t in
  let first = Ufdsu.union t a b in
  Ufdsu.restore t level;
  let second = Ufdsu.union t a b in
  print_s [%message (first : bool) (second : bool)];
  [%expect {| ((first true) (second true)) |}]
;;

let%expect_test "restore to save with no unions is a no-op" =
  let t = Ufdsu.create () in
  let a = Ufdsu.add t in
  let b = Ufdsu.add t in
  ignore (Ufdsu.union t a b : bool);
  let level = Ufdsu.save t in
  Ufdsu.restore t level;
  print_s [%message (Ufdsu.same_class t a b : bool)];
  [%expect {| ("Ufdsu.same_class t a b" true) |}]
;;

(* Reference implementation: naive graph reachability, snapshot-based undo. *)
module Reference = struct
  type t =
    { mutable edges : (int * int) list
    ; mutable stack : (int * int) list list
    }

  let create () = { edges = []; stack = [] }

  let rec connected edges x y visited =
    if x = y
    then true
    else if Hash_set.mem visited x
    then false
    else (
      Hash_set.add visited x;
      List.exists edges ~f:(fun (a, b) ->
        (a = x && connected edges b y visited)
        || (b = x && connected edges a y visited)))
  ;;

  let same_class t x y = connected t.edges x y (Int.Hash_set.create ())

  let union t x y =
    if same_class t x y
    then false
    else (
      t.edges <- (x, y) :: t.edges;
      true)
  ;;

  let push t = t.stack <- t.edges :: t.stack

  let pop t =
    match t.stack with
    | [] -> ()
    | saved :: rest ->
      t.edges <- saved;
      t.stack <- rest
  ;;
end

type operation =
  | Union of int * int
  | Push
  | Pop
[@@deriving sexp_of]

let quickcheck_generator_operation =
  let open Quickcheck.Generator.Let_syntax in
  let gen_elem = Int.gen_incl 0 9 in
  Quickcheck.Generator.union
    [ (let%map x = gen_elem
       and y = gen_elem in
       Union (x, y))
    ; Quickcheck.Generator.return Push
    ; Quickcheck.Generator.return Pop
    ]
;;

let%test_unit "quickcheck matches reference" =
  Quickcheck.test
    ~trials:500
    ~sexp_of:[%sexp_of: operation list]
    (Quickcheck.Generator.list quickcheck_generator_operation)
    ~f:(fun ops ->
      let t = Ufdsu.create () in
      let ref_ = Reference.create () in
      let levels : Ufdsu.Level.t Stack.t = Stack.create () in
      List.iter ops ~f:(fun op ->
        match op with
        | Union (x, y) ->
          let ours = Ufdsu.union t x y in
          let theirs = Reference.union ref_ x y in
          if Bool.( <> ) ours theirs
          then
            failwith (sprintf "union(%d,%d): ours=%b theirs=%b" x y ours theirs);
          (* also cross-check same_class *)
          let sc_ours = Ufdsu.same_class t x y in
          let sc_theirs = Reference.same_class ref_ x y in
          if Bool.( <> ) sc_ours sc_theirs
          then
            failwith
              (sprintf
                 "same_class(%d,%d): ours=%b theirs=%b"
                 x
                 y
                 sc_ours
                 sc_theirs)
        | Push ->
          Stack.push levels (Ufdsu.save t);
          Reference.push ref_
        | Pop ->
          (match Stack.pop levels with
           | None -> ()
           | Some level ->
             Ufdsu.restore t level;
             Reference.pop ref_)))
;;
