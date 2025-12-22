open! Core

(* Define a red-black tree with int keys and unit values *)
module Int_key : Ds.Rb_intf.Key with type t = int = struct
  type t = int

  let compare = Int.compare
  let create_for_rb () = 0
end

module Unit_value : Ds.Rb_intf.Value with type t = unit = struct
  type t = unit

  let create_for_rb () = ()
end

module Rb = Ds.Rb.Make (Int_key) (Unit_value)

(* Operation types for benchmarking *)
type operation =
  | Insert of int
  | Find of int
  | Remove of int
  | Mem of int
  | Min
  | Max
[@@deriving sexp_of, quickcheck]

(* Custom quickcheck generator that produces more realistic distributions *)
let operation_generator ~key_range =
  let open Quickcheck.Generator.Let_syntax in
  let key_gen = Int.gen_incl 0 (key_range - 1) in
  let%bind operation_type = Int.gen_incl 0 5 in
  match operation_type with
  | 0 ->
    let%map key = key_gen in
    Insert key
  | 1 ->
    let%map key = key_gen in
    Find key
  | 2 ->
    let%map key = key_gen in
    Remove key
  | 3 ->
    let%map key = key_gen in
    Mem key
  | 4 -> return Min
  | 5 -> return Max
  | _ -> failwith "unreachable"
;;

(* Generate a random tree of size n *)
let generate_random_tree ~size =
  let tree = Rb.create () in
  let keys = List.init size ~f:Fn.id |> List.permute in
  List.iter keys ~f:(fun key -> Rb.insert tree ~key ~data:());
  tree
;;

(* Execute a single operation on the tree *)
let execute_operation tree op =
  match op with
  | Insert key -> Rb.insert tree ~key ~data:()
  | Find key -> ignore (Rb.find tree key : Rb.Kv_option.t)
  | Remove key -> Rb.remove tree key
  | Mem key -> ignore (Rb.mem tree key : bool)
  | Min -> ignore (Rb.min tree : Rb.Kv_option.t)
  | Max -> ignore (Rb.max tree : Rb.Kv_option.t)
;;

(* Generate n operations using quickcheck *)
let generate_operations ~n ~key_range =
  Quickcheck.random_value
    (Quickcheck.Generator.list_with_length n (operation_generator ~key_range))
;;

(* Benchmark configuration *)
type bench_config =
  { tree_size : int
  ; num_operations : int
  ; key_range : int option
  }

let default_config =
  { tree_size = 1000; num_operations = 100; key_range = None }
;;

let create_config ?tree_size ?num_operations ?key_range () =
  { tree_size = Option.value tree_size ~default:default_config.tree_size
  ; num_operations =
      Option.value num_operations ~default:default_config.num_operations
  ; key_range = Option.first_some key_range default_config.key_range
  }
;;

(* Create a benchmark function that generates a tree and runs operations *)
let make_benchmark_fn ~config =
  let key_range =
    Option.value config.key_range ~default:(config.tree_size * 100)
  in
  let tree = generate_random_tree ~size:config.tree_size in
  let operations = generate_operations ~n:config.num_operations ~key_range in
  fun () -> List.iter operations ~f:(fun op -> execute_operation tree op)
;;

(* Run a single benchmark with the given configuration *)
let run ?benchmark_config ~bench_config ~name () =
  let benchmark_fn = make_benchmark_fn ~config:bench_config in
  Benchmark.run ?config:benchmark_config ~name benchmark_fn
;;

(* Run multiple benchmarks with different tree sizes *)
let run_scaling_benchmark
  ?(benchmark_config = Benchmark.Config.default)
  ?(tree_sizes = [ 100; 1000; 10000 ])
  ?(num_operations = 1000)
  ()
  =
  List.map tree_sizes ~f:(fun tree_size ->
    let name = sprintf "RB tree (n=%d, ops=%d)" tree_size num_operations in
    let bench_config = create_config ~tree_size ~num_operations () in
    let benchmark_fn = make_benchmark_fn ~config:bench_config in
    Benchmark.run ~config:benchmark_config ~name benchmark_fn)
;;

(* Run benchmarks with different operation mixes *)
let run_operation_mix_benchmark
  ?(benchmark_config = Benchmark.Config.default)
  ?(tree_size = 1000)
  ?(num_operations = 100)
  ()
  =
  (* For now, we use the random mix from quickcheck *)
  (* In the future, we could add weighted generators for different mixes *)
  let name =
    sprintf "RB tree mixed ops (n=%d, ops=%d)" tree_size num_operations
  in
  let bench_config = create_config ~tree_size ~num_operations () in
  let benchmark_fn = make_benchmark_fn ~config:bench_config in
  Benchmark.run ~config:benchmark_config ~name benchmark_fn
;;
