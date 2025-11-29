open! Core
open! Ds

let%expect_test "basic insertion and lookup" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  RB.insert t ~key:5 ~data:50;
  RB.insert t ~key:3 ~data:30;
  RB.insert t ~key:7 ~data:70;
  RB.insert t ~key:1 ~data:10;
  RB.insert t ~key:9 ~data:90;
  print_s [%message (RB.find_exn t 5 : int)];
  print_s [%message (RB.find_exn t 3 : int)];
  print_s [%message (RB.find_exn t 7 : int)];
  print_s [%message (RB.find_exn t 1 : int)];
  print_s [%message (RB.find_exn t 9 : int)];
  print_s [%message (RB.mem t 4 : bool)];
  print_s [%message (RB.length t : int)];
  RB.validate t;
  [%expect
    {|
    ("RB.find_exn t 5" 50)
    ("RB.find_exn t 3" 30)
    ("RB.find_exn t 7" 70)
    ("RB.find_exn t 1" 10)
    ("RB.find_exn t 9" 90)
    ("RB.mem t 4" false)
    ("RB.length t" 5) |}]
;;

let%expect_test "iteration" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  RB.insert t ~key:5 ~data:50;
  RB.insert t ~key:3 ~data:30;
  RB.insert t ~key:7 ~data:70;
  RB.insert t ~key:1 ~data:10;
  RB.insert t ~key:9 ~data:90;
  RB.iter t ~f:(fun ~key ~data -> print_s [%message (key : int) (data : int)]);
  RB.validate t;
  [%expect
    {|
    ((key 1) (data 10))
    ((key 3) (data 30))
    ((key 5) (data 50))
    ((key 7) (data 70))
    ((key 9) (data 90)) |}]
;;

let%expect_test "removal" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  RB.insert t ~key:5 ~data:50;
  RB.insert t ~key:3 ~data:30;
  RB.insert t ~key:7 ~data:70;
  RB.insert t ~key:1 ~data:10;
  RB.insert t ~key:9 ~data:90;
  print_s [%message "Before removal" (RB.length t : int)];
  RB.remove t 3;
  print_s [%message "After removing 3" (RB.length t : int)];
  print_s [%message (RB.mem t 3 : bool)];
  RB.remove t 7;
  print_s [%message "After removing 7" (RB.length t : int)];
  print_s [%message (RB.mem t 7 : bool)];
  RB.iter t ~f:(fun ~key ~data -> print_s [%message (key : int) (data : int)]);
  [%expect
    {|
    ("Before removal" ("RB.length t" 5))
    ("After removing 3" ("RB.length t" 4))
    ("RB.mem t 3" false)
    ("After removing 7" ("RB.length t" 3))
    ("RB.mem t 7" false)
    ((key 1) (data 10))
    ((key 5) (data 50))
    ((key 9) (data 90)) |}]
;;

let%expect_test "large random insertions" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  let keys = List.init 100 ~f:(fun i -> i) in
  let shuffled = List.permute keys in
  List.iter shuffled ~f:(fun key -> RB.insert t ~key ~data:(key * 10));
  print_s [%message (RB.length t : int)];
  let all_found =
    List.for_all keys ~f:(fun key ->
      try RB.find_exn t key = key * 10 with
      | _ -> false)
  in
  print_s [%message (all_found : bool)];
  RB.validate t;
  print_endline "Validation passed";
  [%expect
    {|
    ("RB.length t" 100)
    (all_found true)
    Validation passed |}]
;;

let%expect_test "update existing key" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  RB.insert t ~key:5 ~data:50;
  print_s [%message (RB.find_exn t 5 : int)];
  print_s [%message (RB.length t : int)];
  RB.insert t ~key:5 ~data:500;
  print_s [%message "After update" (RB.find_exn t 5 : int)];
  print_s [%message (RB.length t : int)];
  RB.validate t;
  [%expect
    {|
    ("RB.find_exn t 5" 50)
    ("RB.length t" 1)
    ("After update" ("RB.find_exn t 5" 500))
    ("RB.length t" 1) |}]
;;

let%expect_test "fold operation" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  List.iter [ 1; 2; 3; 4; 5 ] ~f:(fun key -> RB.insert t ~key ~data:(key * 10));
  let sum = RB.fold t ~init:0 ~f:(fun ~acc ~key:_ ~data -> acc + data) in
  print_s [%message (sum : int)];
  RB.validate t;
  [%expect {| (sum 150) |}]
;;

let%expect_test "to_array and of_array_exn" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let arr = [| #(5, 50); #(3, 30); #(7, 70); #(1, 10); #(9, 90) |] in
  let t = RB.of_array_exn arr in
  print_s [%message (RB.length t : int)];
  let result = RB.to_array t in
  for i = 0 to Array.length result - 1 do
    let #(key, data) = result.(i) in
    print_s [%message (key : int) (data : int)]
  done;
  RB.validate t;
  [%expect
    {|
    ("RB.length t" 5)
    ((key 1) (data 10))
    ((key 3) (data 30))
    ((key 5) (data 50))
    ((key 7) (data 70))
    ((key 9) (data 90)) |}]
;;

let%expect_test "stress test with many operations" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  for i = 0 to 999 do
    RB.insert t ~key:i ~data:(i * 2)
  done;
  print_s [%message "After 1000 insertions" (RB.length t : int)];
  RB.validate t;
  for i = 0 to 999 do
    if i % 2 = 0 then RB.remove t i
  done;
  print_s [%message "After removing evens" (RB.length t : int)];
  let all_odds_present =
    List.init 500 ~f:(fun i -> (i * 2) + 1)
    |> List.for_all ~f:(fun key ->
      try RB.find_exn t key = key * 2 with
      | _ -> false)
  in
  print_s [%message (all_odds_present : bool)];
  let all_evens_gone =
    List.init 500 ~f:(fun i -> i * 2)
    |> List.for_all ~f:(fun key -> not (RB.mem t key))
  in
  print_s [%message (all_evens_gone : bool)];
  print_endline "Stress test passed";
  [%expect
    {|
    ("After 1000 insertions" ("RB.length t" 1000))
    ("After removing evens" ("RB.length t" 500))
    (all_odds_present true)
    (all_evens_gone true)
    Stress test passed |}]
;;

let%expect_test "empty tree operations" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  print_s [%message (RB.is_empty t : bool)];
  print_s [%message (RB.length t : int)];
  print_s [%message (RB.mem t 5 : bool)];
  RB.validate t;
  let arr = RB.to_array t in
  print_s [%message (Array.length arr : int)];
  [%expect
    {|
    ("RB.is_empty t" true)
    ("RB.length t" 0)
    ("RB.mem t 5" false)
    ("Array.length arr" 0) |}]
;;

let%expect_test "single element operations" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  RB.insert t ~key:42 ~data:84;
  print_s [%message (RB.is_empty t : bool)];
  print_s [%message (RB.length t : int)];
  print_s [%message (RB.mem t 42 : bool)];
  print_s [%message (RB.find_exn t 42 : int)];
  RB.validate t;
  RB.remove t 42;
  print_s [%message "After removal" (RB.is_empty t : bool)];
  print_s [%message (RB.length t : int)];
  print_s [%message (RB.mem t 42 : bool)];
  RB.validate t;
  [%expect
    {|
    ("RB.is_empty t" false)
    ("RB.length t" 1)
    ("RB.mem t 42" true)
    ("RB.find_exn t 42" 84)
    ("After removal" ("RB.is_empty t" true))
    ("RB.length t" 0)
    ("RB.mem t 42" false) |}]
;;

let%expect_test "clear operation" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  for i = 0 to 99 do
    RB.insert t ~key:i ~data:(i * 2)
  done;
  print_s [%message "Before clear" (RB.length t : int)];
  RB.clear t;
  print_s [%message "After clear" (RB.length t : int)];
  print_s [%message (RB.is_empty t : bool)];
  RB.validate t;
  RB.insert t ~key:10 ~data:20;
  print_s [%message "After insert" (RB.length t : int)];
  print_s [%message (RB.find_exn t 10 : int)];
  RB.validate t;
  [%expect
    {|
    ("Before clear" ("RB.length t" 100))
    ("After clear" ("RB.length t" 0))
    ("RB.is_empty t" true)
    ("After insert" ("RB.length t" 1))
    ("RB.find_exn t 10" 20) |}]
;;

let%expect_test "sequential insertions ascending" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  for i = 0 to 49 do
    RB.insert t ~key:i ~data:(i * 2)
  done;
  print_s [%message (RB.length t : int)];
  RB.validate t;
  let all_found =
    List.init 50 ~f:(fun i -> i)
    |> List.for_all ~f:(fun key ->
      try RB.find_exn t key = key * 2 with
      | _ -> false)
  in
  print_s [%message (all_found : bool)];
  [%expect {|
    ("RB.length t" 50)
    (all_found true) |}]
;;

let%expect_test "sequential insertions descending" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  for i = 49 downto 0 do
    RB.insert t ~key:i ~data:(i * 2)
  done;
  print_s [%message (RB.length t : int)];
  RB.validate t;
  let all_found =
    List.init 50 ~f:(fun i -> i)
    |> List.for_all ~f:(fun key ->
      try RB.find_exn t key = key * 2 with
      | _ -> false)
  in
  print_s [%message (all_found : bool)];
  [%expect {|
    ("RB.length t" 50)
    (all_found true) |}]
;;

let%expect_test "remove root repeatedly" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  for i = 0 to 19 do
    RB.insert t ~key:i ~data:(i * 2)
  done;
  print_s [%message "Initial length" (RB.length t : int)];
  RB.validate t;
  for i = 0 to 19 do
    RB.remove t i;
    print_s [%message "After removing" (i : int) (RB.length t : int)]
  done;
  print_s [%message "Final" (RB.is_empty t : bool)];
  RB.validate t;
  [%expect
    {|
    ("Initial length" ("RB.length t" 20))
    ("After removing" (i 0) ("RB.length t" 19))
    ("After removing" (i 1) ("RB.length t" 18))
    ("After removing" (i 2) ("RB.length t" 17))
    ("After removing" (i 3) ("RB.length t" 16))
    ("After removing" (i 4) ("RB.length t" 15))
    ("After removing" (i 5) ("RB.length t" 14))
    ("After removing" (i 6) ("RB.length t" 13))
    ("After removing" (i 7) ("RB.length t" 12))
    ("After removing" (i 8) ("RB.length t" 11))
    ("After removing" (i 9) ("RB.length t" 10))
    ("After removing" (i 10) ("RB.length t" 9))
    ("After removing" (i 11) ("RB.length t" 8))
    ("After removing" (i 12) ("RB.length t" 7))
    ("After removing" (i 13) ("RB.length t" 6))
    ("After removing" (i 14) ("RB.length t" 5))
    ("After removing" (i 15) ("RB.length t" 4))
    ("After removing" (i 16) ("RB.length t" 3))
    ("After removing" (i 17) ("RB.length t" 2))
    ("After removing" (i 18) ("RB.length t" 1))
    ("After removing" (i 19) ("RB.length t" 0))
    (Final ("RB.is_empty t" true)) |}]
;;

let%expect_test "remove non-existent keys" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  for i = 0 to 9 do
    RB.insert t ~key:(i * 2) ~data:(i * 4)
  done;
  print_s [%message (RB.length t : int)];
  RB.remove t 1;
  RB.remove t 3;
  RB.remove t 100;
  print_s [%message "After removing non-existent" (RB.length t : int)];
  RB.validate t;
  [%expect
    {|
    ("RB.length t" 10)
    ("After removing non-existent" ("RB.length t" 10)) |}]
;;

let%expect_test "alternating insert and remove" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  for i = 0 to 99 do
    RB.insert t ~key:i ~data:(i * 2);
    if i % 3 = 0 && i > 0 then RB.remove t (i - 1)
  done;
  print_s [%message (RB.length t : int)];
  RB.validate t;
  [%expect {| ("RB.length t" 67) |}]
;;

let%expect_test "duplicate keys behavior" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  RB.insert t ~key:10 ~data:100;
  RB.insert t ~key:20 ~data:200;
  RB.insert t ~key:30 ~data:300;
  print_s [%message (RB.length t : int)];
  print_s [%message (RB.find_exn t 20 : int)];
  RB.insert t ~key:20 ~data:999;
  print_s [%message "After re-insert" (RB.length t : int)];
  print_s [%message (RB.find_exn t 20 : int)];
  RB.validate t;
  [%expect
    {|
    ("RB.length t" 3)
    ("RB.find_exn t 20" 200)
    ("After re-insert" ("RB.length t" 3))
    ("RB.find_exn t 20" 999) |}]
;;

let%expect_test "find on empty tree" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  (match RB.find_exn t 5 with
   | _ -> print_endline "Found"
   | exception Not_found_s _ -> print_endline "Not found as expected"
   | exception _ -> print_endline "Other exception");
  [%expect {| Not found as expected |}]
;;

let%expect_test "iteration on empty tree" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  let count = ref 0 in
  RB.iter t ~f:(fun ~key:_ ~data:_ -> incr count);
  print_s [%message (!count : int)];
  [%expect {| (!count 0) |}]
;;

let%expect_test "fold on empty tree" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  let result = RB.fold t ~init:42 ~f:(fun ~acc ~key:_ ~data:_ -> acc + 1) in
  print_s [%message (result : int)];
  [%expect {| (result 42) |}]
;;

let%expect_test "large tree all operations" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  let keys = List.init 500 ~f:(fun i -> i) in
  let shuffled = List.permute keys in
  List.iter shuffled ~f:(fun key -> RB.insert t ~key ~data:(key * 10));
  print_s [%message "After insertions" (RB.length t : int)];
  RB.validate t;
  let sum = RB.fold t ~init:0 ~f:(fun ~acc ~key:_ ~data -> acc + data) in
  let expected_sum = 499 * 500 / 2 * 10 in
  print_s
    [%message (sum : int) (expected_sum : int) (sum = expected_sum : bool)];
  let arr = RB.to_array t in
  print_s [%message (Array.length arr : int)];
  let is_sorted =
    let rec check i =
      if i >= Array.length arr - 1
      then true
      else (
        let #(k1, _) = arr.(i) in
        let #(k2, _) = arr.(i + 1) in
        k1 < k2 && check (i + 1))
    in
    if Array.length arr = 0 then true else check 0
  in
  print_s [%message (is_sorted : bool)];
  List.iter (List.take shuffled 250) ~f:(fun key -> RB.remove t key);
  print_s [%message "After removals" (RB.length t : int)];
  RB.validate t;
  [%expect
    {|
    ("After insertions" ("RB.length t" 500))
    ((sum 1247500) (expected_sum 1247500) ("sum = expected_sum" true))
    ("Array.length arr" 500)
    (is_sorted true)
    ("After removals" ("RB.length t" 250)) |}]
;;

let%expect_test "min_exn and max_exn basic" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  RB.insert t ~key:5 ~data:50;
  RB.insert t ~key:3 ~data:30;
  RB.insert t ~key:7 ~data:70;
  RB.insert t ~key:1 ~data:10;
  RB.insert t ~key:9 ~data:90;
  let #(min_key, min_val) = RB.min_exn t in
  let #(max_key, max_val) = RB.max_exn t in
  print_s [%message (min_key : int) (min_val : int)];
  print_s [%message (max_key : int) (max_val : int)];
  print_s [%message (RB.length t : int)];
  RB.validate t;
  [%expect
    {|
    ((min_key 1) (min_val 10))
    ((max_key 9) (max_val 90))
    ("RB.length t" 5) |}]
;;

let%expect_test "min_exn and max_exn on empty tree" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  (match RB.min_exn t with
   | _ -> print_endline "Found min"
   | exception Not_found_s _ -> print_endline "min_exn raised Not_found_s"
   | exception _ -> print_endline "min_exn raised other exception");
  (match RB.max_exn t with
   | _ -> print_endline "Found max"
   | exception Not_found_s _ -> print_endline "max_exn raised Not_found_s"
   | exception _ -> print_endline "max_exn raised other exception");
  [%expect {|
    min_exn raised Not_found_s
    max_exn raised Not_found_s |}]
;;

let%expect_test "min_exn and max_exn single element" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  RB.insert t ~key:42 ~data:100;
  let #(min_key, min_val) = RB.min_exn t in
  let #(max_key, max_val) = RB.max_exn t in
  print_s [%message (min_key : int) (min_val : int)];
  print_s [%message (max_key : int) (max_val : int)];
  [%expect
    {|
    ((min_key 42) (min_val 100))
    ((max_key 42) (max_val 100)) |}]
;;

let%expect_test "pop_min_exn basic" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  RB.insert t ~key:5 ~data:50;
  RB.insert t ~key:3 ~data:30;
  RB.insert t ~key:7 ~data:70;
  RB.insert t ~key:1 ~data:10;
  RB.insert t ~key:9 ~data:90;
  print_s [%message "Initial" (RB.length t : int)];
  let #(min_key, min_val) = RB.pop_min_exn t in
  print_s [%message "Popped" (min_key : int) (min_val : int)];
  print_s [%message "After pop" (RB.length t : int)];
  print_s [%message (RB.mem t 1 : bool)];
  RB.validate t;
  let #(new_min_key, new_min_val) = RB.min_exn t in
  print_s [%message "New min" (new_min_key : int) (new_min_val : int)];
  [%expect
    {|
    (Initial ("RB.length t" 5))
    (Popped (min_key 1) (min_val 10))
    ("After pop" ("RB.length t" 4))
    ("RB.mem t 1" false)
    ("New min" (new_min_key 3) (new_min_val 30)) |}]
;;

let%expect_test "pop_max_exn basic" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  RB.insert t ~key:5 ~data:50;
  RB.insert t ~key:3 ~data:30;
  RB.insert t ~key:7 ~data:70;
  RB.insert t ~key:1 ~data:10;
  RB.insert t ~key:9 ~data:90;
  print_s [%message "Initial" (RB.length t : int)];
  let #(max_key, max_val) = RB.pop_max_exn t in
  print_s [%message "Popped" (max_key : int) (max_val : int)];
  print_s [%message "After pop" (RB.length t : int)];
  print_s [%message (RB.mem t 9 : bool)];
  RB.validate t;
  let #(new_max_key, new_max_val) = RB.max_exn t in
  print_s [%message "New max" (new_max_key : int) (new_max_val : int)];
  [%expect
    {|
    (Initial ("RB.length t" 5))
    (Popped (max_key 9) (max_val 90))
    ("After pop" ("RB.length t" 4))
    ("RB.mem t 9" false)
    ("New max" (new_max_key 7) (new_max_val 70)) |}]
;;

let%expect_test "pop all elements via pop_min_exn" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  List.iter [ 5; 3; 7; 1; 9; 2; 4; 6; 8 ] ~f:(fun i ->
    RB.insert t ~key:i ~data:(i * 10));
  print_s [%message "Initial" (RB.length t : int)];
  let popped = ref [] in
  while not (RB.is_empty t) do
    let #(key, _val) = RB.pop_min_exn t in
    popped := key :: !popped;
    RB.validate t
  done;
  let popped_list = List.rev !popped in
  print_s [%message (popped_list : int list)];
  print_s [%message "Final" (RB.length t : int) (RB.is_empty t : bool)];
  [%expect
    {|
    (Initial ("RB.length t" 9))
    (popped_list (1 2 3 4 5 6 7 8 9))
    (Final ("RB.length t" 0) ("RB.is_empty t" true)) |}]
;;

let%expect_test "pop all elements via pop_max_exn" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  List.iter [ 5; 3; 7; 1; 9; 2; 4; 6; 8 ] ~f:(fun i ->
    RB.insert t ~key:i ~data:(i * 10));
  print_s [%message "Initial" (RB.length t : int)];
  let popped = ref [] in
  while not (RB.is_empty t) do
    let #(key, _val) = RB.pop_max_exn t in
    popped := key :: !popped;
    RB.validate t
  done;
  let popped_list = !popped in
  print_s [%message (popped_list : int list)];
  print_s [%message "Final" (RB.length t : int) (RB.is_empty t : bool)];
  [%expect
    {|
    (Initial ("RB.length t" 9))
    (popped_list (1 2 3 4 5 6 7 8 9))
    (Final ("RB.length t" 0) ("RB.is_empty t" true)) |}]
;;

let%expect_test "pop_min_exn on empty tree" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  (match RB.pop_min_exn t with
   | _ -> print_endline "Popped"
   | exception Not_found_s _ -> print_endline "pop_min_exn raised Not_found_s"
   | exception _ -> print_endline "pop_min_exn raised other exception");
  [%expect {| pop_min_exn raised Not_found_s |}]
;;

let%expect_test "pop_max_exn on empty tree" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  (match RB.pop_max_exn t with
   | _ -> print_endline "Popped"
   | exception Not_found_s _ -> print_endline "pop_max_exn raised Not_found_s"
   | exception _ -> print_endline "pop_max_exn raised other exception");
  [%expect {| pop_max_exn raised Not_found_s |}]
;;

let%expect_test "alternating pop_min and pop_max" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  List.iter (List.range 1 21) ~f:(fun i -> RB.insert t ~key:i ~data:(i * 10));
  print_s [%message "Initial" (RB.length t : int)];
  let #(min1, _) = RB.pop_min_exn t in
  let #(max1, _) = RB.pop_max_exn t in
  let #(min2, _) = RB.pop_min_exn t in
  let #(max2, _) = RB.pop_max_exn t in
  print_s [%message (min1 : int) (max1 : int) (min2 : int) (max2 : int)];
  print_s [%message "After pops" (RB.length t : int)];
  RB.validate t;
  let #(new_min, _) = RB.min_exn t in
  let #(new_max, _) = RB.max_exn t in
  print_s [%message (new_min : int) (new_max : int)];
  [%expect
    {|
    (Initial ("RB.length t" 20))
    ((min1 1) (max1 20) (min2 2) (max2 19))
    ("After pops" ("RB.length t" 16))
    ((new_min 3) (new_max 18)) |}]
;;

let%expect_test "optional find - basic" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  RB.insert t ~key:5 ~data:50;
  RB.insert t ~key:3 ~data:30;
  RB.insert t ~key:7 ~data:70;
  let result = RB.find t 5 in
  print_s [%message (RB.Kv_option.is_some result : bool)];
  let #(key, value) = RB.Kv_option.value_exn result in
  print_s [%message (key : int) (value : int)];
  let not_found = RB.find t 10 in
  print_s [%message (RB.Kv_option.is_none not_found : bool)];
  [%expect
    {|
    ("RB.Kv_option.is_some result" true)
    ((key 5) (value 50))
    ("RB.Kv_option.is_none not_found" true) |}]
;;

let%expect_test "optional find - empty tree" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  let result = RB.find t 5 in
  print_s [%message (RB.Kv_option.is_none result : bool)];
  [%expect {| ("RB.Kv_option.is_none result" true) |}]
;;

let%expect_test "optional min and max" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  let empty_min = RB.min t in
  let empty_max = RB.max t in
  print_s [%message (RB.Kv_option.is_none empty_min : bool)];
  print_s [%message (RB.Kv_option.is_none empty_max : bool)];
  RB.insert t ~key:5 ~data:50;
  RB.insert t ~key:3 ~data:30;
  RB.insert t ~key:7 ~data:70;
  let min_result = RB.min t in
  let max_result = RB.max t in
  let #(min_key, min_val) = RB.Kv_option.value_exn min_result in
  let #(max_key, max_val) = RB.Kv_option.value_exn max_result in
  print_s [%message (min_key : int) (min_val : int)];
  print_s [%message (max_key : int) (max_val : int)];
  [%expect
    {|
    ("RB.Kv_option.is_none empty_min" true)
    ("RB.Kv_option.is_none empty_max" true)
    ((min_key 3) (min_val 30))
    ((max_key 7) (max_val 70)) |}]
;;

let%expect_test "optional pop_min and pop_max" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  let empty_pop_min = RB.pop_min t in
  print_s [%message (RB.Kv_option.is_none empty_pop_min : bool)];
  RB.insert t ~key:5 ~data:50;
  RB.insert t ~key:3 ~data:30;
  RB.insert t ~key:7 ~data:70;
  print_s [%message "Initial" (RB.length t : int)];
  let min_result = RB.pop_min t in
  let #(min_key, min_val) = RB.Kv_option.value_exn min_result in
  print_s [%message "Popped min" (min_key : int) (min_val : int)];
  print_s [%message "After pop_min" (RB.length t : int)];
  let max_result = RB.pop_max t in
  let #(max_key, max_val) = RB.Kv_option.value_exn max_result in
  print_s [%message "Popped max" (max_key : int) (max_val : int)];
  print_s [%message "After pop_max" (RB.length t : int)];
  RB.validate t;
  [%expect
    {|
    ("RB.Kv_option.is_none empty_pop_min" true)
    (Initial ("RB.length t" 3))
    ("Popped min" (min_key 3) (min_val 30))
    ("After pop_min" ("RB.length t" 2))
    ("Popped max" (max_key 7) (max_val 70))
    ("After pop_max" ("RB.length t" 1)) |}]
;;

let%expect_test "iterator - basic traversal" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  List.iter [ 5; 3; 7; 1; 9; 2; 8 ] ~f:(fun k ->
    RB.insert t ~key:k ~data:(k * 10));
  let iter = RB.Iter.create t in
  let rec loop () =
    if not (RB.Iter.is_done iter)
    then (
      let result = RB.Iter.next iter in
      let #(key, value) = RB.Kv_option.value_exn result in
      print_s [%message (key : int) (value : int)];
      loop ())
  in
  loop ();
  [%expect
    {|
    ((key 1) (value 10))
    ((key 2) (value 20))
    ((key 3) (value 30))
    ((key 5) (value 50))
    ((key 7) (value 70))
    ((key 8) (value 80))
    ((key 9) (value 90)) |}]
;;

let%expect_test "iterator - empty tree" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  let iter = RB.Iter.create t in
  print_s [%message (RB.Iter.is_done iter : bool)];
  let result = RB.Iter.next iter in
  print_s [%message (RB.Kv_option.is_none result : bool)];
  [%expect
    {|
    ("RB.Iter.is_done iter" true)
    ("RB.Kv_option.is_none result" true) |}]
;;

let%expect_test "iterator - peek without advancing" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  List.iter [ 5; 3; 7 ] ~f:(fun k -> RB.insert t ~key:k ~data:(k * 10));
  let iter = RB.Iter.create t in
  let peek1 = RB.Iter.peek iter in
  let #(key1, val1) = RB.Kv_option.value_exn peek1 in
  print_s [%message "First peek" (key1 : int) (val1 : int)];
  let peek2 = RB.Iter.peek iter in
  let #(key2, val2) = RB.Kv_option.value_exn peek2 in
  print_s [%message "Second peek (should be same)" (key2 : int) (val2 : int)];
  let next_result = RB.Iter.next iter in
  let #(key3, val3) = RB.Kv_option.value_exn next_result in
  print_s [%message "After next" (key3 : int) (val3 : int)];
  let peek3 = RB.Iter.peek iter in
  let #(key4, val4) = RB.Kv_option.value_exn peek3 in
  print_s [%message "Peek after next" (key4 : int) (val4 : int)];
  [%expect
    {|
    ("First peek" (key1 3) (val1 30))
    ("Second peek (should be same)" (key2 3) (val2 30))
    ("After next" (key3 3) (val3 30))
    ("Peek after next" (key4 5) (val4 50)) |}]
;;

let%expect_test "iterator - single element" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  RB.insert t ~key:42 ~data:100;
  let iter = RB.Iter.create t in
  print_s [%message (RB.Iter.is_done iter : bool)];
  let result = RB.Iter.next iter in
  let #(key, value) = RB.Kv_option.value_exn result in
  print_s [%message (key : int) (value : int)];
  print_s [%message (RB.Iter.is_done iter : bool)];
  let empty_result = RB.Iter.next iter in
  print_s [%message (RB.Kv_option.is_none empty_result : bool)];
  [%expect
    {|
    ("RB.Iter.is_done iter" false)
    ((key 42) (value 100))
    ("RB.Iter.is_done iter" true)
    ("RB.Kv_option.is_none empty_result" true) |}]
;;

let%expect_test "iterator - create_from exact match" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  List.iter [ 1; 3; 5; 7; 9 ] ~f:(fun k -> RB.insert t ~key:k ~data:(k * 10));
  let iter = RB.Iter.create_from t 5 in
  print_s [%message "Starting from 5"];
  let rec loop () =
    if not (RB.Iter.is_done iter)
    then (
      let result = RB.Iter.next iter in
      let #(key, value) = RB.Kv_option.value_exn result in
      print_s [%message (key : int) (value : int)];
      loop ())
  in
  loop ();
  [%expect
    {|
    "Starting from 5"
    ((key 5) (value 50))
    ((key 7) (value 70))
    ((key 9) (value 90)) |}]
;;

let%expect_test "iterator - create_from greater than all" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  List.iter [ 1; 3; 5 ] ~f:(fun k -> RB.insert t ~key:k ~data:(k * 10));
  let iter = RB.Iter.create_from t 10 in
  print_s [%message "Starting from 10 (greater than all)"];
  print_s [%message (RB.Iter.is_done iter : bool)];
  [%expect
    {|
    "Starting from 10 (greater than all)"
    ("RB.Iter.is_done iter" true) |}]
;;

let%expect_test "iterator - create_from less than all" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  List.iter [ 5; 7; 9 ] ~f:(fun k -> RB.insert t ~key:k ~data:(k * 10));
  let iter = RB.Iter.create_from t 0 in
  print_s [%message "Starting from 0 (less than all)"];
  let rec loop () =
    if not (RB.Iter.is_done iter)
    then (
      let result = RB.Iter.next iter in
      let #(key, value) = RB.Kv_option.value_exn result in
      print_s [%message (key : int) (value : int)];
      loop ())
  in
  loop ();
  [%expect
    {|
    "Starting from 0 (less than all)"
    ((key 5) (value 50))
    ((key 7) (value 70))
    ((key 9) (value 90)) |}]
;;

let%expect_test "iterator - create_from between elements" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  List.iter [ 1; 3; 5; 7; 9; 11 ] ~f:(fun k ->
    RB.insert t ~key:k ~data:(k * 10));
  let iter = RB.Iter.create_from t 6 in
  print_s [%message "Starting from 6 (between 5 and 7)"];
  let rec loop () =
    if not (RB.Iter.is_done iter)
    then (
      let result = RB.Iter.next iter in
      let #(key, value) = RB.Kv_option.value_exn result in
      print_s [%message (key : int) (value : int)];
      loop ())
  in
  loop ();
  [%expect
    {|
    "Starting from 6 (between 5 and 7)"
    ((key 7) (value 70))
    ((key 9) (value 90))
    ((key 11) (value 110)) |}]
;;

let%expect_test "iterator - create_from on empty tree" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  let iter = RB.Iter.create_from t 5 in
  print_s [%message (RB.Iter.is_done iter : bool)];
  [%expect {| ("RB.Iter.is_done iter" true) |}]
;;

let%expect_test "iterator - create_from large tree" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  for i = 0 to 99 do
    RB.insert t ~key:i ~data:(i * 10)
  done;
  let iter = RB.Iter.create_from t 50 in
  print_s [%message "Starting from 50"];
  let count = ref 0 in
  let rec loop () =
    if not (RB.Iter.is_done iter)
    then (
      let result = RB.Iter.next iter in
      let #(key, _) = RB.Kv_option.value_exn result in
      if !count < 5 then print_s [%message (key : int)];
      incr count;
      loop ())
  in
  loop ();
  print_s [%message "Total elements from 50 onwards" (!count : int)];
  [%expect
    {|
    "Starting from 50"
    (key 50)
    (key 51)
    (key 52)
    (key 53)
    (key 54)
    ("Total elements from 50 onwards" (!count 50)) |}]
;;

let%expect_test "iterator - multiple iterators on same tree" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  List.iter [ 1; 2; 3; 4; 5 ] ~f:(fun k -> RB.insert t ~key:k ~data:(k * 10));
  let iter1 = RB.Iter.create t in
  let iter2 = RB.Iter.create_from t 3 in
  let r1 = RB.Iter.next iter1 in
  let #(k1, _) = RB.Kv_option.value_exn r1 in
  print_s [%message "iter1 first" (k1 : int)];
  let r2 = RB.Iter.next iter2 in
  let #(k2, _) = RB.Kv_option.value_exn r2 in
  print_s [%message "iter2 first" (k2 : int)];
  let r3 = RB.Iter.next iter1 in
  let #(k3, _) = RB.Kv_option.value_exn r3 in
  print_s [%message "iter1 second" (k3 : int)];
  let r4 = RB.Iter.next iter2 in
  let #(k4, _) = RB.Kv_option.value_exn r4 in
  print_s [%message "iter2 second" (k4 : int)];
  [%expect
    {|
    ("iter1 first" (k1 1))
    ("iter2 first" (k2 3))
    ("iter1 second" (k3 2))
    ("iter2 second" (k4 4)) |}]
;;

let%expect_test "iterator - stress test with create_from" =
  let module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end
  in
  let module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end
  in
  let module RB = Rb.Make [@kind value value] (Int_key) (Int_value) in
  let t = RB.create () in
  for i = 0 to 999 do
    RB.insert t ~key:i ~data:(i * 2)
  done;
  (* Test multiple starting points *)
  let test_starting_point start =
    let iter = RB.Iter.create_from t start in
    let first_result = RB.Iter.next iter in
    if RB.Kv_option.is_some first_result
    then (
      let #(first_key, _) = RB.Kv_option.value_exn first_result in
      (* Verify first key is >= start *)
      if first_key < start
      then
        print_s
          [%message "ERROR: first_key < start" (first_key : int) (start : int)])
  in
  (* Test various starting points *)
  List.iter [ 0; 100; 250; 500; 750; 999; 1000 ] ~f:test_starting_point;
  print_endline "All create_from tests passed";
  [%expect {| All create_from tests passed |}]
;;

(* Quickcheck tests comparing RB tree to Int.Map *)
module Quickcheck_modules = struct
  module Int_key = struct
    type t = int

    let compare = Int.compare
    let create_for_rb () = 0
  end

  module Int_value = struct
    type t = int

    let create_for_rb () = 0
  end

  module RB = Rb.Make [@kind value value] (Int_key) (Int_value)

  type operation =
    | Insert of int * int
    | Remove of int
    | Find of int
    | Mem of int
    | Min
    | Max
    | PopMin
    | PopMax
    | Length
  [@@deriving sexp_of, quickcheck]

  let compare_to_map rb map =
    let rb_length = RB.length rb in
    let map_length = Map.length map in
    if rb_length <> map_length
    then
      failwith (sprintf "Length mismatch: RB=%d, Map=%d" rb_length map_length);
    Map.iteri map ~f:(fun ~key ~data ->
      match RB.find rb key |> RB.Kv_option.is_some with
      | false -> failwith (sprintf "Key %d in Map but not in RB" key)
      | true ->
        let rb_value = RB.find_exn rb key in
        if rb_value <> data
        then
          failwith
            (sprintf
               "Value mismatch for key %d: RB=%d, Map=%d"
               key
               rb_value
               data));
    RB.iter rb ~f:(fun ~key ~data ->
      match Map.find map key with
      | None -> failwith (sprintf "Key %d in RB but not in Map" key)
      | Some map_value ->
        if data <> map_value
        then
          failwith
            (sprintf
               "Value mismatch for key %d: RB=%d, Map=%d"
               key
               data
               map_value));
    RB.validate rb
  ;;

  let execute_operation rb map op =
    match op with
    | Insert (key, value) ->
      RB.insert rb ~key ~data:value;
      Map.set map ~key ~data:value
    | Remove key ->
      RB.remove rb key;
      Map.remove map key
    | Find key ->
      let rb_result = RB.find rb key in
      let map_result = Map.find map key in
      (match RB.Kv_option.is_some rb_result, map_result with
       | true, Some map_value ->
         let #(_, rb_value) = RB.Kv_option.value_exn rb_result in
         if rb_value <> map_value
         then
           failwith
             (sprintf
                "Find mismatch for key %d: RB=%d, Map=%d"
                key
                rb_value
                map_value)
       | false, None -> ()
       | true, None ->
         failwith (sprintf "Find: key %d in RB but not in Map" key)
       | false, Some _ ->
         failwith (sprintf "Find: key %d in Map but not in RB" key));
      map
    | Mem key ->
      let rb_result : bool = RB.mem rb key in
      let map_result : bool = Map.mem map key in
      if Bool.( <> ) rb_result map_result
      then
        failwith
          (sprintf
             "Mem mismatch for key %d: RB=%b, Map=%b"
             key
             rb_result
             map_result);
      map
    | Min ->
      let rb_result = RB.min rb in
      let map_result = Map.min_elt map in
      (match RB.Kv_option.is_some rb_result, map_result with
       | true, Some (map_key, map_value) ->
         let #(rb_key, rb_value) = RB.Kv_option.value_exn rb_result in
         if rb_key <> map_key || rb_value <> map_value
         then
           failwith
             (sprintf
                "Min mismatch: RB=(%d,%d), Map=(%d,%d)"
                rb_key
                rb_value
                map_key
                map_value)
       | false, None -> ()
       | true, None -> failwith "Min: RB has min but Map doesn't"
       | false, Some _ -> failwith "Min: Map has min but RB doesn't");
      map
    | Max ->
      let rb_result = RB.max rb in
      let map_result = Map.max_elt map in
      (match RB.Kv_option.is_some rb_result, map_result with
       | true, Some (map_key, map_value) ->
         let #(rb_key, rb_value) = RB.Kv_option.value_exn rb_result in
         if rb_key <> map_key || rb_value <> map_value
         then
           failwith
             (sprintf
                "Max mismatch: RB=(%d,%d), Map=(%d,%d)"
                rb_key
                rb_value
                map_key
                map_value)
       | false, None -> ()
       | true, None -> failwith "Max: RB has max but Map doesn't"
       | false, Some _ -> failwith "Max: Map has max but RB doesn't");
      map
    | PopMin ->
      let rb_result = RB.pop_min rb in
      let map_result = Map.min_elt map in
      (match RB.Kv_option.is_some rb_result, map_result with
       | true, Some (map_key, map_value) ->
         let #(rb_key, rb_value) = RB.Kv_option.value_exn rb_result in
         if rb_key <> map_key || rb_value <> map_value
         then
           failwith
             (sprintf
                "PopMin mismatch: RB=(%d,%d), Map=(%d,%d)"
                rb_key
                rb_value
                map_key
                map_value);
         Map.remove map map_key
       | false, None -> map
       | true, None -> failwith "PopMin: RB has min but Map doesn't"
       | false, Some _ -> failwith "PopMin: Map has min but RB doesn't")
    | PopMax ->
      let rb_result = RB.pop_max rb in
      let map_result = Map.max_elt map in
      (match RB.Kv_option.is_some rb_result, map_result with
       | true, Some (map_key, map_value) ->
         let #(rb_key, rb_value) = RB.Kv_option.value_exn rb_result in
         if rb_key <> map_key || rb_value <> map_value
         then
           failwith
             (sprintf
                "PopMax mismatch: RB=(%d,%d), Map=(%d,%d)"
                rb_key
                rb_value
                map_key
                map_value);
         Map.remove map map_key
       | false, None -> map
       | true, None -> failwith "PopMax: RB has max but Map doesn't"
       | false, Some _ -> failwith "PopMax: Map has max but RB doesn't")
    | Length ->
      let rb_len = RB.length rb in
      let map_len = Map.length map in
      if rb_len <> map_len
      then failwith (sprintf "Length mismatch: RB=%d, Map=%d" rb_len map_len);
      map
  ;;
end

let%test_unit "random operations match Map" =
  let open Quickcheck_modules in
  Quickcheck.test
    ~sexp_of:[%sexp_of: operation list]
    (Quickcheck.Generator.list_with_length
       100
       [%quickcheck.generator: operation])
    ~f:(fun operations ->
      let rb = RB.create () in
      let map = ref (Map.empty (module Int)) in
      List.iter operations ~f:(fun op ->
        map := execute_operation rb !map op;
        compare_to_map rb !map))
;;

let%test_unit "insert then remove sequence" =
  let open Quickcheck_modules in
  Quickcheck.test
    ~sexp_of:[%sexp_of: (int * int) list * int list]
    (Quickcheck.Generator.tuple2
       (Quickcheck.Generator.list [%quickcheck.generator: int * int])
       (Quickcheck.Generator.list [%quickcheck.generator: int]))
    ~f:(fun (inserts, removes) ->
      let rb = RB.create () in
      let map = ref (Map.empty (module Int)) in
      (* Insert phase *)
      List.iter inserts ~f:(fun (key, value) ->
        RB.insert rb ~key ~data:value;
        map := Map.set !map ~key ~data:value;
        compare_to_map rb !map);
      (* Remove phase *)
      List.iter removes ~f:(fun key ->
        RB.remove rb key;
        map := Map.remove !map key;
        compare_to_map rb !map))
;;

let%test_unit "alternating insert/remove" =
  let open Quickcheck_modules in
  Quickcheck.test
    ~sexp_of:[%sexp_of: (int * int) list]
    (Quickcheck.Generator.list [%quickcheck.generator: int * int])
    ~f:(fun pairs ->
      let rb = RB.create () in
      let map = ref (Map.empty (module Int)) in
      List.iter pairs ~f:(fun (key, value) ->
        (* Insert *)
        RB.insert rb ~key ~data:value;
        map := Map.set !map ~key ~data:value;
        compare_to_map rb !map;
        (* Remove if exists *)
        if Map.mem !map key
        then (
          RB.remove rb key;
          map := Map.remove !map key;
          compare_to_map rb !map)))
;;

let%test_unit "pop_min drains tree correctly" =
  let open Quickcheck_modules in
  Quickcheck.test
    ~sexp_of:[%sexp_of: (int * int) list]
    (Quickcheck.Generator.list [%quickcheck.generator: int * int])
    ~f:(fun inserts ->
      let rb = RB.create () in
      let map = ref (Map.empty (module Int)) in
      List.iter inserts ~f:(fun (key, value) ->
        RB.insert rb ~key ~data:value;
        map := Map.set !map ~key ~data:value);
      while not (RB.is_empty rb) do
        let rb_result = RB.pop_min rb in
        let map_min = Map.min_elt !map in
        match map_min with
        | None -> failwith "Map is empty but RB is not"
        | Some (min_key, min_value) ->
          let #(rb_key, rb_value) = RB.Kv_option.value_exn rb_result in
          if rb_key <> min_key || rb_value <> min_value
          then
            failwith
              (sprintf
                 "PopMin mismatch: RB=(%d,%d), Map=(%d,%d)"
                 rb_key
                 rb_value
                 min_key
                 min_value);
          map := Map.remove !map min_key;
          compare_to_map rb !map
      done;
      if not (Map.is_empty !map) then failwith "RB is empty but Map is not")
;;

let%test_unit "pop_max drains tree correctly" =
  let open Quickcheck_modules in
  Quickcheck.test
    ~sexp_of:[%sexp_of: (int * int) list]
    (Quickcheck.Generator.list [%quickcheck.generator: int * int])
    ~f:(fun inserts ->
      let rb = RB.create () in
      let map = ref (Map.empty (module Int)) in
      (* Insert all *)
      List.iter inserts ~f:(fun (key, value) ->
        RB.insert rb ~key ~data:value;
        map := Map.set !map ~key ~data:value);
      (* Pop all maxes *)
      while not (RB.is_empty rb) do
        let rb_result = RB.pop_max rb in
        let map_max = Map.max_elt !map in
        match map_max with
        | None -> failwith "Map is empty but RB is not"
        | Some (max_key, max_value) ->
          let #(rb_key, rb_value) = RB.Kv_option.value_exn rb_result in
          if rb_key <> max_key || rb_value <> max_value
          then
            failwith
              (sprintf
                 "PopMax mismatch: RB=(%d,%d), Map=(%d,%d)"
                 rb_key
                 rb_value
                 max_key
                 max_value);
          map := Map.remove !map max_key;
          compare_to_map rb !map
      done;
      if not (Map.is_empty !map) then failwith "RB is empty but Map is not")
;;

let%test_unit "large random operation sequence" =
  let open Quickcheck_modules in
  Quickcheck.test
    ~trials:100
    ~sexp_of:[%sexp_of: operation list]
    (Quickcheck.Generator.list [%quickcheck.generator: operation])
    ~f:(fun operations ->
      let rb = RB.create () in
      let map = ref (Map.empty (module Int)) in
      List.iter operations ~f:(fun op -> map := execute_operation rb !map op);
      compare_to_map rb !map)
;;
