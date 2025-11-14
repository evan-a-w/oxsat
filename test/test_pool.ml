open! Core
open! Ds

[@@@warning "-69"]

let%expect_test "basic pool allocation and iteration" =
  let module Elt = struct
    type t = #{ x : int; y : int }

    let create_for_pool () = #{ x = 0; y = 0 }
  end
  in
  let module P = Pool.Make [@kind value & value] (Elt) in
  let pool = P.create ~chunk_size:4 () in
  (* Allocate some elements *)
  let p1 = P.alloc pool in
  let p2 = P.alloc pool in
  let p3 = P.alloc pool in
  P.set pool p1 #{ x = 100; y = 1 };
  P.set pool p2 #{ x = 200; y = 2 };
  P.set pool p3 #{ x = 300; y = 3 };
  (* Iterate and collect values *)
  let values = ref [] in
  P.iter pool ~f:(fun ptr -> values := (P.get pool ptr).#x :: !values);
  print_s [%message (List.sort ~compare:Int.compare !values : int list)];
  [%expect {| ("List.sort ~compare:Int.compare (!values)" (100 200 300)) |}]
;;

let%expect_test "pool iteration after free" =
  let module Elt = struct
    type t = #{ x : int; y : int }

    let create_for_pool () = #{ x = 0; y = 0 }
  end
  in
  let module P = Pool.Make [@kind value & value] (Elt) in
  let pool = P.create ~chunk_size:4 () in
  (* Allocate elements *)
  let p1 = P.alloc pool in
  let p2 = P.alloc pool in
  let p3 = P.alloc pool in
  let p4 = P.alloc pool in
  P.set pool p1 #{ x = 10; y = 1 };
  P.set pool p2 #{ x = 20; y = 2 };
  P.set pool p3 #{ x = 30; y = 3 };
  P.set pool p4 #{ x = 40; y = 4 };
  (* Free some elements *)
  P.free pool p2;
  P.free pool p4;
  (* Iterate - should only see p1 and p3 *)
  let values = ref [] in
  P.iter pool ~f:(fun ptr -> values := (P.get pool ptr).#x :: !values);
  print_s [%message (List.sort ~compare:Int.compare !values : int list)];
  [%expect {| ("List.sort ~compare:Int.compare (!values)" (10 30)) |}]
;;

let%expect_test "pool iteration with alloc after free" =
  let module Elt = struct
    type t = #{ x : int; y : int }

    let create_for_pool () = #{ x = 0; y = 0 }
  end
  in
  let module P = Pool.Make [@kind value & value] (Elt) in
  let pool = P.create ~chunk_size:4 () in
  (* Allocate elements *)
  let p1 = P.alloc pool in
  let p2 = P.alloc pool in
  let p3 = P.alloc pool in
  P.set pool p1 #{ x = 10; y = 1 };
  P.set pool p2 #{ x = 20; y = 2 };
  P.set pool p3 #{ x = 30; y = 3 };
  (* Free middle element *)
  P.free pool p2;
  (* Allocate new element (should reuse p2's slot) *)
  let p4 = P.alloc pool in
  P.set pool p4 #{ x = 40; y = 4 };
  (* Iterate - should see p1, p3, p4 *)
  let values = ref [] in
  P.iter pool ~f:(fun ptr -> values := (P.get pool ptr).#x :: !values);
  print_s [%message (List.sort ~compare:Int.compare !values : int list)];
  [%expect {| ("List.sort ~compare:Int.compare (!values)" (10 30 40)) |}]
;;

let%expect_test "pool iteration across multiple chunks" =
  let module Elt = struct
    type t = #{ x : int; y : int }

    let create_for_pool () = #{ x = 0; y = 0 }
  end
  in
  let module P = Pool.Make [@kind value & value] (Elt) in
  let pool = P.create ~chunk_size:4 () in
  (* Allocate more elements than chunk_size to trigger multiple chunks *)
  let p0 = P.alloc pool in
  let p1 = P.alloc pool in
  let p2 = P.alloc pool in
  let p3 = P.alloc pool in
  let p4 = P.alloc pool in
  let p5 = P.alloc pool in
  let p6 = P.alloc pool in
  P.set pool p0 #{ x = 0; y = 0 };
  P.set pool p1 #{ x = 10; y = 1 };
  P.set pool p2 #{ x = 20; y = 2 };
  P.set pool p3 #{ x = 30; y = 3 };
  P.set pool p4 #{ x = 40; y = 4 };
  P.set pool p5 #{ x = 50; y = 5 };
  P.set pool p6 #{ x = 60; y = 6 };
  (* Free some elements from different chunks *)
  P.free pool p1;
  P.free pool p4;
  (* Iterate and collect values *)
  let values = ref [] in
  P.iter pool ~f:(fun ptr -> values := (P.get pool ptr).#x :: !values);
  print_s [%message (List.sort ~compare:Int.compare !values : int list)];
  [%expect {| ("List.sort ~compare:Int.compare (!values)" (0 20 30 50 60)) |}]
;;

let%expect_test "empty pool iteration" =
  let module Elt = struct
    type t = #{ x : int; y : int }

    let create_for_pool () = #{ x = 0; y = 0 }
  end
  in
  let module P = Pool.Make [@kind value & value] (Elt) in
  let pool = P.create ~chunk_size:4 () in
  (* Don't allocate anything *)
  let count = ref 0 in
  P.iter pool ~f:(fun _ptr -> count := !count + 1);
  print_s [%message (!count : int)];
  [%expect {| (!count 0) |}]
;;

let%expect_test "pool iteration after allocating and freeing all" =
  let module Elt = struct
    type t = #{ x : int; y : int }

    let create_for_pool () = #{ x = 0; y = 0 }
  end
  in
  let module P = Pool.Make [@kind value & value] (Elt) in
  let pool = P.create ~chunk_size:4 () in
  (* Allocate elements *)
  let p1 = P.alloc pool in
  let p2 = P.alloc pool in
  let p3 = P.alloc pool in
  P.set pool p1 #{ x = 10; y = 1 };
  P.set pool p2 #{ x = 20; y = 2 };
  P.set pool p3 #{ x = 30; y = 3 };
  (* Free all elements *)
  P.free pool p1;
  P.free pool p2;
  P.free pool p3;
  (* Iterate - should see nothing *)
  let count = ref 0 in
  P.iter pool ~f:(fun _ptr -> count := !count + 1);
  print_s [%message (!count : int)];
  [%expect {| (!count 0) |}]
;;
