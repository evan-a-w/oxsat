open! Core
open! Ds

(* Helpers for managing an external trail, mirroring how
   uninterpreted_functions.ml will use this. Undo_entry.t has an unboxed product
   layout so we box it as a tuple for list storage. *)

type trail = (int * int * bool) list ref

let trail_save (trail : trail) = List.length !trail

let trail_restore t (trail : trail) level =
  while List.length !trail > level do
    match !trail with
    | [] -> ()
    | (child, new_root, rank_incremented) :: rest ->
      Ufdsu.undo
        t
        ~undo_entry:#{ Ufdsu.Undo_entry.child; new_root; rank_incremented };
      trail := rest
  done
;;

let do_union t (trail : trail) x y =
  match%optional_u (Ufdsu.union t x y : Ufdsu.Undo_entry.Option_u.t) with
  | None -> false
  | Some entry ->
    let #{ Ufdsu.Undo_entry.child; new_root; rank_incremented } = entry in
    trail := (child, new_root, rank_incremented) :: !trail;
    true
;;

let%expect_test "basic save and restore" =
  let t = Ufdsu.create () in
  let trail = ref [] in
  let a = Ufdsu.add t in
  let b = Ufdsu.add t in
  let c = Ufdsu.add t in
  ignore (do_union t trail a b : bool);
  let level = trail_save trail in
  ignore (do_union t trail b c : bool);
  print_s
    [%message
      "before restore"
        ~ab:(Ufdsu.same_class t a b : bool)
        ~bc:(Ufdsu.same_class t b c : bool)
        ~ac:(Ufdsu.same_class t a c : bool)];
  trail_restore t trail level;
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
  let trail = ref [] in
  let a = Ufdsu.add t in
  let b = Ufdsu.add t in
  let c = Ufdsu.add t in
  let d = Ufdsu.add t in
  let l0 = trail_save trail in
  ignore (do_union t trail a b : bool);
  let l1 = trail_save trail in
  ignore (do_union t trail c d : bool);
  ignore (do_union t trail a c : bool);
  print_s [%message "all merged" ~abcd:(Ufdsu.same_class t a d : bool)];
  trail_restore t trail l1;
  print_s
    [%message
      "after restore to l1"
        ~ab:(Ufdsu.same_class t a b : bool)
        ~cd:(Ufdsu.same_class t c d : bool)
        ~ac:(Ufdsu.same_class t a c : bool)];
  trail_restore t trail l0;
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

let%expect_test "union after restore returns This again" =
  let t = Ufdsu.create () in
  let trail = ref [] in
  let a = Ufdsu.add t in
  let b = Ufdsu.add t in
  let level = trail_save trail in
  let first = do_union t trail a b in
  trail_restore t trail level;
  let second = do_union t trail a b in
  print_s [%message (first : bool) (second : bool)];
  [%expect {| ((first true) (second true)) |}]
;;

let%expect_test "restore to save with no unions is a no-op" =
  let t = Ufdsu.create () in
  let trail = ref [] in
  let a = Ufdsu.add t in
  let b = Ufdsu.add t in
  ignore (do_union t trail a b : bool);
  let level = trail_save trail in
  trail_restore t trail level;
  print_s [%message (Ufdsu.same_class t a b : bool)];
  [%expect {| ("Ufdsu.same_class t a b" true) |}]
;;

let class_members t x =
  let members = ref [] in
  Ufdsu.iter_class t x ~f:(fun m -> members := m :: !members);
  List.sort !members ~compare:Int.compare
;;

let%expect_test "iter_class basic" =
  let t = Ufdsu.create () in
  let trail = ref [] in
  let a = Ufdsu.add t in
  let b = Ufdsu.add t in
  let c = Ufdsu.add t in
  ignore (do_union t trail a b : bool);
  ignore (do_union t trail b c : bool);
  print_s [%message (class_members t a : int list)];
  print_s [%message (class_members t b : int list)];
  [%expect
    {|
    ("class_members t a" (0 1 2))
    ("class_members t b" (0 1 2))
    |}]
;;

let%expect_test "iter_class restore splits lists correctly" =
  let t = Ufdsu.create () in
  let trail = ref [] in
  let a = Ufdsu.add t in
  let b = Ufdsu.add t in
  let c = Ufdsu.add t in
  ignore (do_union t trail a b : bool);
  let level = trail_save trail in
  ignore (do_union t trail b c : bool);
  print_s [%message "before restore" ~members:(class_members t a : int list)];
  trail_restore t trail level;
  print_s
    [%message
      "after restore"
        ~ab_class:(class_members t a : int list)
        ~c_class:(class_members t c : int list)];
  [%expect
    {|
    ("before restore" (members (0 1 2)))
    ("after restore" (ab_class (0 1)) (c_class (2)))
    |}]
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
      let trail = ref [] in
      let ref_ = Reference.create () in
      let level_stack : int Stack.t = Stack.create () in
      List.iter ops ~f:(fun op ->
        match op with
        | Union (x, y) ->
          let ours = do_union t trail x y in
          let theirs = Reference.union ref_ x y in
          if Bool.( <> ) ours theirs
          then
            failwith (sprintf "union(%d,%d): ours=%b theirs=%b" x y ours theirs);
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
                 sc_theirs);
          let class_x = class_members t x in
          List.iter (List.init 10 ~f:Fun.id) ~f:(fun m ->
            let in_iter = List.mem class_x m ~equal:Int.equal in
            let in_same = Ufdsu.same_class t x m in
            if Bool.( <> ) in_iter in_same
            then
              failwith
                (sprintf
                   "iter_class/same_class mismatch x=%d m=%d in_iter=%b \
                    in_same=%b"
                   x
                   m
                   in_iter
                   in_same))
        | Push ->
          Stack.push level_stack (trail_save trail);
          Reference.push ref_
        | Pop ->
          (match Stack.pop level_stack with
           | None -> ()
           | Some level ->
             trail_restore t trail level;
             Reference.pop ref_)))
;;
