open! Core
open! Ds

module Int_key = struct
  type t = int

  let hash = Hashtbl.hash
  let equal = Int.equal
  let create_for_hash_table () = 0
end

module Int_value = struct
  type t = int

  let create_for_hash_table () = 0
end

module Int_table = Hash_table.Make [@kind value value] (Int_key) (Int_value)

let value_opt t key =
  let kv = Int_table.find t key in
  if Int_table.Kv_option.is_none kv
  then None
  else (
    let #(_, data) = Int_table.Kv_option.value_exn kv in
    Some data)
;;

let entries_sorted t =
  let entries = ref [] in
  Int_table.iter t ~f:(fun ~key ~data -> entries := (key, data) :: !entries);
  List.sort !entries ~compare:[%compare: int * int]
;;

let%expect_test "basic operations" =
  let t = Int_table.create () in
  Int_table.insert t ~key:5 ~data:50;
  Int_table.insert t ~key:3 ~data:30;
  Int_table.insert t ~key:7 ~data:70;
  Int_table.insert t ~key:3 ~data:300;
  print_s
    [%message
      "state"
        ~value_5:(value_opt t 5 : int option)
        ~value_3:(value_opt t 3 : int option)
        ~value_4:(value_opt t 4 : int option)
        ~mem_7:(Int_table.mem t 7 : bool)
        ~mem_10:(Int_table.mem t 10 : bool)
        ~length:(Int_table.length t : int)];
  print_s [%message (entries_sorted t : (int * int) list)];
  [%expect
    {|
    (state (value_5 (50)) (value_3 (300)) (value_4 ()) (mem_7 true)
     (mem_10 false) (length 3))
    ("entries_sorted t" ((3 300) (5 50) (7 70)))
    |}]
;;

let%expect_test "removals and reinsertion" =
  let t = Int_table.create ~capacity:4 () in
  List.iter [ 0; 1; 2; 3 ] ~f:(fun key -> Int_table.insert t ~key ~data:(key * 10));
  Int_table.remove t 1;
  Int_table.remove t 2;
  Int_table.insert t ~key:5 ~data:50;
  Int_table.insert t ~key:6 ~data:60;
  print_s
    [%message
      "state"
        ~length:(Int_table.length t : int)
        ~mem_1:(Int_table.mem t 1 : bool)
        ~mem_2:(Int_table.mem t 2 : bool)
        ~mem_5:(Int_table.mem t 5 : bool)
        ~mem_6:(Int_table.mem t 6 : bool)];
  print_s [%message (entries_sorted t : (int * int) list)];
  [%expect
    {|
    (state (length 4) (mem_1 false) (mem_2 false) (mem_5 true) (mem_6 true))
    ("entries_sorted t" ((0 0) (3 30) (5 50) (6 60)))
    |}]
;;

let%expect_test "clear resets table" =
  let t = Int_table.create () in
  List.iter [ 1; 2; 3 ] ~f:(fun key -> Int_table.insert t ~key ~data:(key * 10));
  let has_load () = Float.(>) (Int_table.load_factor t) 0. in
  print_s
    [%message
      "before"
        ~length:(Int_table.length t : int)
        ~is_empty:(Int_table.is_empty t : bool)
        ~has_load:(has_load () : bool)];
  Int_table.clear t;
  print_s
    [%message
      "after"
        ~length:(Int_table.length t : int)
        ~is_empty:(Int_table.is_empty t : bool)
        ~has_load:(has_load () : bool)];
  [%expect
    {|
    (before (length 3) (is_empty false) (has_load true))
    (after (length 0) (is_empty true) (has_load false))
    |}]
;;

(* Quickcheck tests comparing hash table to Core's Int.Table *)
module Quickcheck_modules = struct
  module Int_key = struct
    type t = int

    let hash = Hashtbl.hash
    let equal = Int.equal
    let create_for_hash_table () = 0
  end

  module Int_value = struct
    type t = int

    let create_for_hash_table () = 0
  end

  module Table = Hash_table.Make [@kind value value] (Int_key) (Int_value)

  type operation =
    | Insert of int * int
    | Remove of int
    | Find of int
    | Mem of int
    | Clear
    | Length
  [@@deriving sexp_of, quickcheck]

  let compare_to_reference table reference =
    let table_length = Table.length table in
    let reference_length = Hashtbl.length reference in
    if table_length <> reference_length
    then
      failwith
        (sprintf
           "length mismatch table=%d reference=%d"
           table_length
           reference_length);
    Hashtbl.iteri reference ~f:(fun ~key ~data ->
      let kv = Table.find table key in
      if Table.Kv_option.is_none kv
      then failwith (sprintf "key %d missing from hash table" key)
      else (
        let #(_, table_data) = Table.Kv_option.value_exn kv in
        if table_data <> data
        then
          failwith
            (sprintf "value mismatch for key %d table=%d reference=%d" key table_data data)));
    Table.iter table ~f:(fun ~key ~data ->
      match Hashtbl.find reference key with
      | None -> failwith (sprintf "key %d missing from reference" key)
      | Some reference_data ->
        if reference_data <> data
        then
          failwith
            (sprintf
               "value mismatch for key %d table=%d reference=%d"
               key
               data
               reference_data))
  ;;

  let execute_operation table reference = function
    | Insert (key, value) ->
      Table.insert table ~key ~data:value;
      Hashtbl.set reference ~key ~data:value
    | Remove key ->
      Table.remove table key;
      Hashtbl.remove reference key
    | Clear ->
      Table.clear table;
      Hashtbl.clear reference
    | Length ->
      let table_length = Table.length table in
      let reference_length = Hashtbl.length reference in
      if table_length <> reference_length
      then
        failwith
          (sprintf
             "length mismatch table=%d reference=%d"
             table_length
             reference_length)
    | Mem key ->
      let ours = Table.mem table key in
      let theirs = Hashtbl.mem reference key in
      if Bool.( <> ) ours theirs
      then failwith (sprintf "mem mismatch key=%d table=%b reference=%b" key ours theirs)
    | Find key ->
      let ours = Table.find table key in
      let theirs = Hashtbl.find reference key in
      (match Table.Kv_option.is_some ours, theirs with
       | true, Some ref_value ->
         let #(_, ours_value) = Table.Kv_option.value_exn ours in
         if ours_value <> ref_value
         then
           failwith
             (sprintf
                "find mismatch key=%d table=%d reference=%d"
                key
                ours_value
                ref_value)
       | false, None -> ()
       | true, None -> failwith (sprintf "key %d found in table but not reference" key)
       | false, Some _ -> failwith (sprintf "key %d found in reference but not table" key))
  ;;
end

let%test_unit "quickcheck matches Int.Table" =
  let open Quickcheck_modules in
  Quickcheck.test
    ~trials:100
    ~sexp_of:[%sexp_of: operation list]
    (Quickcheck.Generator.list [%quickcheck.generator: operation])
    ~f:(fun operations ->
      let table = Table.create () in
      let reference = Int.Table.create () in
      List.iter operations ~f:(fun operation ->
        execute_operation table reference operation;
        compare_to_reference table reference))
;;
