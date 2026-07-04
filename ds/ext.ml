open! Core
module Typerep = Typerep_lib.Std.Typerep

external raw_create_pool : int -> 'a -> nativeint = "oxsat_ext_create_pool"
external raw_pool_outstanding : nativeint -> int = "oxsat_ext_pool_outstanding"
external raw_alloc : nativeint -> int = "oxsat_ext_alloc"
external raw_free : int -> unit = "oxsat_ext_free"
external raw_get : int -> 'a = "oxsat_ext_get"
external raw_set : int -> 'a @ local -> unit = "oxsat_ext_set"
external raw_is_freed : int -> bool = "oxsat_ext_is_freed"

module Pool = struct
  type 'a t =
    { raw : nativeint
    ; typerep : 'a Typerep.t
    ; chunk_size : int
    }

  let create ?(chunk_size = 4096) typerep ~default =
    if chunk_size <= 0
    then invalid_arg "Ext.Pool.create: chunk_size must be positive";
    { raw = raw_create_pool chunk_size default; typerep; chunk_size }
  ;;

  let raw t =
    if Nativeint.equal t.raw Nativeint.zero
    then failwith "Ext.Pool: pool has been destroyed";
    t.raw
  ;;

  let typerep t = t.typerep
  let chunk_size t = t.chunk_size
  let outstanding t = raw_pool_outstanding (raw t)
end

type 'a pool = 'a Pool.t
type 'a t = int

let create_pool = Pool.create
let alloc pool = raw_alloc (Pool.raw pool)

let alloc_set pool (local_ value) =
  let t = alloc pool in
  raw_set t value;
  t
;;

let free = raw_free
let set t (local_ value) = raw_set t value
let get = raw_get
let is_freed = raw_is_freed

let%expect_test "alloc get set free" =
  let pool = create_pool ~chunk_size:2 [%typerep_of: int] ~default:0 in
  let a = alloc_set pool 1 in
  let b = alloc pool in
  set b 2;
  print_s [%sexp (Pool.outstanding pool : int)];
  print_s [%sexp ((get a, get b) : int * int)];
  free a;
  print_s [%sexp (Pool.outstanding pool : int), (is_freed a : bool)];
  let c = alloc_set pool 3 in
  print_s [%sexp (Pool.outstanding pool : int), (get c : int)];
  [%expect {|
    2
    (1 2)
    (1 true)
    (2 3) |}]
;;

module Test_record = struct
  type t =
    { mutable i : int
    ; mutable s : string
    }
  [@@deriving sexp_of, typerep]
end

let%expect_test "external block can be mutated and roots values" =
  let pool =
    create_pool
      ~chunk_size:1
      [%typerep_of: Test_record.t]
      ~default:{ Test_record.i = 0; s = "default" }
  in
  let ext = alloc pool in
  let value : Test_record.t = get ext in
  value.i <- 1;
  value.s <- String.init 1000 ~f:(fun _ -> 'x');
  Gc.full_major ();
  let value : Test_record.t = get ext in
  print_s [%sexp (value.i : int), (String.length value.s : int)];
  [%expect {|
    (1 1000) |}]
;;
