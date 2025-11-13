open! Core
open! Ds
open! Unboxed

[@@@warning "-69"]

(* Helper to convert bitset to sorted list for testing *)
let to_list bs =
  let result = ref [] in
  Bitset.iter_set_bits bs ~f:(fun i -> result := i :: !result);
  List.sort ~compare:Int.compare !result
;;

(* Helper to convert or_null to option for pretty printing *)
let or_null_to_option (type a) (x : a or_null) : a option =
  match x with
  | Null -> None
  | This v -> Some v
;;

let%expect_test "create empty bitset" =
  let bs = Bitset.create () in
  print_s [%message (Bitset.is_empty bs : bool) (Bitset.popcount bs : int)];
  [%expect {| (("Bitset.is_empty bs" true) ("Bitset.popcount bs" 0)) |}]
;;

let%expect_test "set and get single bit" =
  let bs = Bitset.create () in
  Bitset.set bs 5;
  print_s [%message (Bitset.get bs 5 : bool) (Bitset.get bs 4 : bool)];
  [%expect {| (("Bitset.get bs 5" true) ("Bitset.get bs 4" false)) |}]
;;

let%expect_test "set multiple bits" =
  let bs = Bitset.create () in
  Bitset.set bs 1;
  Bitset.set bs 5;
  Bitset.set bs 10;
  print_s [%message (to_list bs : int list) (Bitset.popcount bs : int)];
  [%expect {| (("to_list bs" (1 5 10)) ("Bitset.popcount bs" 3)) |}]
;;

let%expect_test "clear bit" =
  let bs = Bitset.create () in
  Bitset.set bs 5;
  print_s [%message "after set" (Bitset.get bs 5 : bool)];
  Bitset.clear bs 5;
  print_s [%message "after clear" (Bitset.get bs 5 : bool)];
  [%expect {|
    ("after set" ("Bitset.get bs 5" true))
    ("after clear" ("Bitset.get bs 5" false))
    |}]
;;

let%expect_test "toggle bit" =
  let bs = Bitset.create () in
  print_s [%message "initial" (Bitset.get bs 3 : bool)];
  Bitset.toggle bs 3;
  print_s [%message "after first toggle" (Bitset.get bs 3 : bool)];
  Bitset.toggle bs 3;
  print_s [%message "after second toggle" (Bitset.get bs 3 : bool)];
  [%expect {|
    (initial ("Bitset.get bs 3" false))
    ("after first toggle" ("Bitset.get bs 3" true))
    ("after second toggle" ("Bitset.get bs 3" false))
    |}]
;;

let%expect_test "clear_all" =
  let bs = Bitset.create () in
  Bitset.set bs 1;
  Bitset.set bs 5;
  Bitset.set bs 100;
  print_s [%message "before clear_all" (Bitset.popcount bs : int)];
  Bitset.clear_all bs;
  print_s [%message "after clear_all" (Bitset.popcount bs : int) (Bitset.is_empty bs : bool)];
  [%expect {|
    ("before clear_all" ("Bitset.popcount bs" 3))
    ("after clear_all" ("Bitset.popcount bs" 0) ("Bitset.is_empty bs" true))
    |}]
;;

let%expect_test "set_all" =
  let bs = Bitset.create () in
  Bitset.set_all bs ~up_to:10;
  print_s [%message (to_list bs : int list) (Bitset.popcount bs : int)];
  [%expect {| (("to_list bs" (0 1 2 3 4 5 6 7 8 9)) ("Bitset.popcount bs" 10)) |}]
;;

let%expect_test "copy bitset" =
  let bs1 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 5;
  let bs2 = Bitset.copy bs1 in
  print_s [%message "original" (to_list bs1 : int list)];
  print_s [%message "copy" (to_list bs2 : int list)];
  (* Modify original and verify copy is unchanged *)
  Bitset.set bs1 10;
  print_s [%message "after modifying original" (to_list bs1 : int list) (to_list bs2 : int list)];
  [%expect {|
    (original ("to_list bs1" (1 5)))
    (copy ("to_list bs2" (1 5)))
    ("after modifying original" ("to_list bs1" (1 5 10)) ("to_list bs2" (1 5)))
    |}]
;;

let%expect_test "find_first_set - empty bitset" =
  let bs = Bitset.create () in
  let result = Bitset.find_first_set bs ~start_pos:0 in
  print_s [%message (or_null_to_option result : int option)];
  [%expect {| ("or_null_to_option result" ()) |}]
;;

let%expect_test "find_first_set - single bit" =
  let bs = Bitset.create () in
  Bitset.set bs 5;
  let result = Bitset.find_first_set bs ~start_pos:0 in
  print_s [%message (or_null_to_option result : int option)];
  [%expect {| ("or_null_to_option result" (5)) |}]
;;

let%expect_test "find_first_set - multiple bits" =
  let bs = Bitset.create () in
  Bitset.set bs 10;
  Bitset.set bs 20;
  Bitset.set bs 30;
  let result1 = Bitset.find_first_set bs ~start_pos:0 in
  let result2 = Bitset.find_first_set bs ~start_pos:15 in
  let result3 = Bitset.find_first_set bs ~start_pos:25 in
  let result4 = Bitset.find_first_set bs ~start_pos:35 in
  print_s
    [%message
      (or_null_to_option result1 : int option)
        (or_null_to_option result2 : int option)
        (or_null_to_option result3 : int option)
        (or_null_to_option result4 : int option)];
  [%expect {|
    (("or_null_to_option result1" (10)) ("or_null_to_option result2" (20))
     ("or_null_to_option result3" (30)) ("or_null_to_option result4" ()))
    |}]
;;

let%expect_test "find_first_set - start_pos beyond all bits" =
  let bs = Bitset.create () in
  Bitset.set bs 5;
  Bitset.set bs 10;
  let result = Bitset.find_first_set bs ~start_pos:100 in
  print_s [%message (or_null_to_option result : int option)];
  [%expect {| ("or_null_to_option result" ()) |}]
;;

let%expect_test "find_first_set - bit at start_pos" =
  let bs = Bitset.create () in
  Bitset.set bs 10;
  let result = Bitset.find_first_set bs ~start_pos:10 in
  print_s [%message (or_null_to_option result : int option)];
  [%expect {| ("or_null_to_option result" (10)) |}]
;;

let%expect_test "find_first_clear - empty bitset" =
  let bs = Bitset.create () in
  let result = Bitset.find_first_clear bs ~start_pos:0 in
  print_s [%message (or_null_to_option result : int option)];
  [%expect {| ("or_null_to_option result" (0)) |}]
;;

let%expect_test "find_first_clear - some bits set" =
  let bs = Bitset.create () in
  Bitset.set bs 0;
  Bitset.set bs 1;
  Bitset.set bs 2;
  let result = Bitset.find_first_clear bs ~start_pos:0 in
  print_s [%message (or_null_to_option result : int option)];
  [%expect {| ("or_null_to_option result" (3)) |}]
;;

let%expect_test "iter_set_bits" =
  let bs = Bitset.create () in
  Bitset.set bs 2;
  Bitset.set bs 5;
  Bitset.set bs 8;
  let collected = ref [] in
  Bitset.iter_set_bits bs ~f:(fun i -> collected := i :: !collected);
  print_s [%message (List.sort ~compare:Int.compare !collected : int list)];
  [%expect {| ("List.sort ~compare:Int.compare (!collected)" (2 5 8)) |}]
;;

let%expect_test "iter_set_bits - empty" =
  let bs = Bitset.create () in
  let count = ref 0 in
  Bitset.iter_set_bits bs ~f:(fun _ -> incr count);
  print_s [%message (!count : int)];
  [%expect {| (!count 0) |}]
;;

let%expect_test "land_ (bitwise AND)" =
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 2;
  Bitset.set bs1 3;
  Bitset.set bs2 2;
  Bitset.set bs2 3;
  Bitset.set bs2 4;
  let result = Bitset.land_ bs1 bs2 in
  print_s [%message (to_list result : int list)];
  [%expect {| ("to_list result" (2 3)) |}]
;;

let%expect_test "land_inplace (bitwise AND in-place)" =
  let dest = Bitset.create () in
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 2;
  Bitset.set bs1 3;
  Bitset.set bs2 2;
  Bitset.set bs2 3;
  Bitset.set bs2 4;
  Bitset.land_inplace ~dest bs1 bs2;
  print_s [%message (to_list dest : int list)];
  [%expect {| ("to_list dest" (2 3)) |}]
;;

let%expect_test "lor_ (bitwise OR)" =
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 2;
  Bitset.set bs2 3;
  Bitset.set bs2 4;
  let result = Bitset.lor_ bs1 bs2 in
  print_s [%message (to_list result : int list)];
  [%expect {| ("to_list result" (1 2 3 4)) |}]
;;

let%expect_test "lor_inplace (bitwise OR in-place)" =
  let dest = Bitset.create () in
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 2;
  Bitset.set bs2 3;
  Bitset.set bs2 4;
  Bitset.lor_inplace ~dest bs1 bs2;
  print_s [%message (to_list dest : int list)];
  [%expect {| ("to_list dest" (1 2 3 4)) |}]
;;

let%expect_test "lxor_ (bitwise XOR)" =
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 2;
  Bitset.set bs1 3;
  Bitset.set bs2 2;
  Bitset.set bs2 3;
  Bitset.set bs2 4;
  let result = Bitset.lxor_ bs1 bs2 in
  print_s [%message (to_list result : int list)];
  [%expect {| ("to_list result" (1 4)) |}]
;;

let%expect_test "lxor_inplace (bitwise XOR in-place)" =
  let dest = Bitset.create () in
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 2;
  Bitset.set bs1 3;
  Bitset.set bs2 2;
  Bitset.set bs2 3;
  Bitset.set bs2 4;
  Bitset.lxor_inplace ~dest bs1 bs2;
  print_s [%message (to_list dest : int list)];
  [%expect {| ("to_list dest" (1 4)) |}]
;;

let%expect_test "lnot_ (bitwise NOT) - limited range" =
  let bs = Bitset.create () in
  Bitset.set bs 0;
  Bitset.set bs 2;
  Bitset.set bs 4;
  let result = Bitset.lnot_ bs in
  (* NOT will flip bits up to the capacity, let's just check a few *)
  print_s
    [%message
      (Bitset.get result 0 : bool)
        (Bitset.get result 1 : bool)
        (Bitset.get result 2 : bool)
        (Bitset.get result 3 : bool)
        (Bitset.get result 4 : bool)];
  [%expect {|
    (("Bitset.get result 0" false) ("Bitset.get result 1" true)
     ("Bitset.get result 2" false) ("Bitset.get result 3" true)
     ("Bitset.get result 4" false))
    |}]
;;

let%expect_test "lnot_inplace (bitwise NOT in-place)" =
  let bs = Bitset.create () in
  Bitset.set bs 0;
  Bitset.set bs 2;
  Bitset.set bs 4;
  Bitset.lnot_inplace bs;
  print_s
    [%message
      (Bitset.get bs 0 : bool)
        (Bitset.get bs 1 : bool)
        (Bitset.get bs 2 : bool)
        (Bitset.get bs 3 : bool)
        (Bitset.get bs 4 : bool)];
  [%expect {|
    (("Bitset.get bs 0" false) ("Bitset.get bs 1" true) ("Bitset.get bs 2" false)
     ("Bitset.get bs 3" true) ("Bitset.get bs 4" false))
    |}]
;;

let%expect_test "to_set_bits_array and of_set_bits_array" =
  let bs1 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 5;
  Bitset.set bs1 10;
  let arr = Bitset.to_set_bits_array bs1 in
  print_s [%message (arr : int array)];
  let bs2 = Bitset.of_set_bits_array arr in
  print_s [%message (to_list bs2 : int list)];
  [%expect {|
    (arr (1 5 10))
    ("to_list bs2" (1 5 10))
    |}]
;;

let%expect_test "to_set_bits_array - empty" =
  let bs = Bitset.create () in
  let arr = Bitset.to_set_bits_array bs in
  print_s [%message (arr : int array)];
  [%expect {| (arr ()) |}]
;;

let%expect_test "of_set_bits_array - empty" =
  let bs = Bitset.of_set_bits_array [||] in
  print_s [%message (Bitset.is_empty bs : bool)];
  [%expect {| ("Bitset.is_empty bs" true) |}]
;;

let%expect_test "capacity grows as needed" =
  let bs = Bitset.create ~size:10 () in
  print_s [%message "initial capacity" (Bitset.capacity bs : int)];
  Bitset.set bs 5;
  print_s [%message "after setting bit 5" (Bitset.capacity bs : int)];
  Bitset.set bs 100;
  print_s [%message "after setting bit 100" (Bitset.capacity bs : int)];
  [%expect {|
    ("initial capacity" ("Bitset.capacity bs" 64))
    ("after setting bit 5" ("Bitset.capacity bs" 64))
    ("after setting bit 100" ("Bitset.capacity bs" 128))
    |}]
;;

let%expect_test "setting same bit multiple times" =
  let bs = Bitset.create () in
  Bitset.set bs 5;
  Bitset.set bs 5;
  Bitset.set bs 5;
  print_s [%message (Bitset.popcount bs : int) (to_list bs : int list)];
  [%expect {| (("Bitset.popcount bs" 1) ("to_list bs" (5))) |}]
;;

let%expect_test "clearing unset bit is no-op" =
  let bs = Bitset.create () in
  Bitset.clear bs 5;
  print_s [%message (Bitset.is_empty bs : bool)];
  [%expect {| ("Bitset.is_empty bs" true) |}]
;;

let%expect_test "large bit indices" =
  let bs = Bitset.create () in
  Bitset.set bs 1000;
  Bitset.set bs 2000;
  Bitset.set bs 5000;
  print_s [%message (Bitset.popcount bs : int) (to_list bs : int list)];
  [%expect {| (("Bitset.popcount bs" 3) ("to_list bs" (1000 2000 5000))) |}]
;;

let%expect_test "fold_set_bits" =
  let bs = Bitset.create () in
  Bitset.set bs 1;
  Bitset.set bs 3;
  Bitset.set bs 5;
  let sum = Bitset.fold_set_bits bs ~init:0 ~f:(fun acc i -> acc + i) in
  print_s [%message (sum : int)];
  [%expect {| (sum 9) |}]
;;

let%expect_test "fold_set_bits - empty" =
  let bs = Bitset.create () in
  let sum = Bitset.fold_set_bits bs ~init:0 ~f:(fun acc i -> acc + i) in
  print_s [%message (sum : int)];
  [%expect {| (sum 0) |}]
;;

let%expect_test "to_set_bits_array matches iter_set_bits" =
  let bs = Bitset.create () in
  Bitset.set bs 1;
  Bitset.set bs 5;
  Bitset.set bs 10;
  let arr = Bitset.to_set_bits_array bs in
  let list_result = to_list bs in
  print_s [%message (arr : int array) (list_result : int list)];
  [%expect {| ((arr (1 5 10)) (list_result (1 5 10))) |}]
;;

let%expect_test "diff (set difference)" =
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 2;
  Bitset.set bs1 3;
  Bitset.set bs1 4;
  Bitset.set bs2 2;
  Bitset.set bs2 4;
  Bitset.set bs2 5;
  let result = Bitset.diff bs1 bs2 in
  print_s [%message (to_list result : int list)];
  [%expect {| ("to_list result" (1 3)) |}]
;;

let%expect_test "diff_inplace (set difference in-place)" =
  let dest = Bitset.create () in
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 2;
  Bitset.set bs1 3;
  Bitset.set bs1 4;
  Bitset.set bs2 2;
  Bitset.set bs2 4;
  Bitset.set bs2 5;
  Bitset.diff_inplace ~dest bs1 bs2;
  print_s [%message (to_list dest : int list)];
  [%expect {| ("to_list dest" (1 3)) |}]
;;

let%expect_test "diff - no overlap" =
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 2;
  Bitset.set bs2 3;
  Bitset.set bs2 4;
  let result = Bitset.diff bs1 bs2 in
  print_s [%message (to_list result : int list)];
  [%expect {| ("to_list result" (1 2)) |}]
;;

let%expect_test "diff - complete overlap" =
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 2;
  Bitset.set bs2 1;
  Bitset.set bs2 2;
  let result = Bitset.diff bs1 bs2 in
  print_s [%message (to_list result : int list) (Bitset.is_empty result : bool)];
  [%expect {| (("to_list result" ()) ("Bitset.is_empty result" true)) |}]
;;

let%expect_test "diff - empty first set" =
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs2 1;
  Bitset.set bs2 2;
  let result = Bitset.diff bs1 bs2 in
  print_s [%message (to_list result : int list) (Bitset.is_empty result : bool)];
  [%expect {| (("to_list result" ()) ("Bitset.is_empty result" true)) |}]
;;

let%expect_test "diff - empty second set" =
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 2;
  let result = Bitset.diff bs1 bs2 in
  print_s [%message (to_list result : int list)];
  [%expect {| ("to_list result" (1 2)) |}]
;;

let%expect_test "diff - both empty" =
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  let result = Bitset.diff bs1 bs2 in
  print_s [%message (Bitset.is_empty result : bool)];
  [%expect {| ("Bitset.is_empty result" true) |}]
;;

let%expect_test "diff - superset removal" =
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 1;
  Bitset.set bs1 2;
  Bitset.set bs1 3;
  Bitset.set bs2 1;
  Bitset.set bs2 2;
  Bitset.set bs2 3;
  Bitset.set bs2 4;
  Bitset.set bs2 5;
  let result = Bitset.diff bs1 bs2 in
  print_s [%message (to_list result : int list) (Bitset.is_empty result : bool)];
  [%expect {| (("to_list result" ()) ("Bitset.is_empty result" true)) |}]
;;

let%expect_test "diff - large indices" =
  let bs1 = Bitset.create () in
  let bs2 = Bitset.create () in
  Bitset.set bs1 100;
  Bitset.set bs1 200;
  Bitset.set bs1 300;
  Bitset.set bs2 200;
  Bitset.set bs2 400;
  let result = Bitset.diff bs1 bs2 in
  print_s [%message (to_list result : int list)];
  [%expect {| ("to_list result" (100 300)) |}]
;;
