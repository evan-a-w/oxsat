open! Core
open! Import

module T = struct
  type t = Bitset.t Tf_pair.t [@@deriving sexp]

  let create_for_vec () = Tf_pair.create (fun (_ : bool) -> Bitset.create ())
  let create_for_pool = create_for_vec
  let copy t = Tf_pair.create (fun b -> Tf_pair.get t b |> Bitset.copy)
end

include T

let is_tautology t = Bitset.popcount (Bitset.land_ t.#Tf_pair.t t.#f) > 0

let is_satisfied t ~assignments =
  let satisfied_vars =
    Bitset.land_ (Tf_pair.get t true) (Tf_pair.get assignments true)
  in
  let satisfied_false_vars =
    Bitset.land_ (Tf_pair.get t false) (Tf_pair.get assignments false)
  in
  Bitset.lor_inplace ~dest:satisfied_vars satisfied_vars satisfied_false_vars;
  Bitset.popcount satisfied_vars > 0
;;

let iter_literals t ~(f : _ @ local) =
  Tf_pair.iteri t ~f:(fun ~key:value ~data:bitset ->
    Bitset.iter_set_bits bitset ~f:(fun var -> Literal.create ~var ~value |> f)
    [@nontail])
  [@nontail]
;;

let%template literals_list (t : t @ local) : int list @ m =
  (let acc =
     (Bitset.fold_set_bits [@kind value] [@alloc a])
       t.#t
       ~init:[]
       ~f:(fun l i -> i :: l [@exclave_if_stack a])
   in
   (Bitset.fold_set_bits [@kind value] [@alloc a]) t.#f ~init:acc ~f:(fun l i ->
     -i :: l [@exclave_if_stack a])
   [@nontail])
  [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let contains t ~var =
  Tf_pair.fold t ~init:false ~f:(fun acc bs -> Bitset.get bs var || acc)
  [@nontail]
;;

let contains_literal t ~literal =
  Bitset.get (Tf_pair.get t (Literal.value literal)) (Literal.var literal)
;;

let unit_literal t ~assignments =
  if is_satisfied t ~assignments
  then Literal.Option.none ()
  else (
    let assigned = Bitset.lor_ assignments.#Tf_pair.t assignments.#Tf_pair.f in
    let tr = Bitset.diff t.#Tf_pair.t assigned in
    let fa = Bitset.diff t.#f assigned in
    match Bitset.popcount tr, Bitset.popcount fa with
    | 1, 0 ->
      Bitset.find_first_set tr ~start_pos:0
      |> Or_null.value_exn
      |> Literal.of_int
      |> Literal.Option.some
    | 0, 1 ->
      Bitset.find_first_set fa ~start_pos:0
      |> Or_null.value_exn
      |> Int.neg
      |> Literal.of_int
      |> Literal.Option.some
    | _, _ -> Literal.Option.none ())
;;

let clear t = Tf_pair.iter t ~f:Bitset.clear_all

let can_resolve t ~other ~on_var =
  let t_pos = Bitset.get t.#Tf_pair.t on_var in
  let t_neg = Bitset.get t.#f on_var in
  let other_pos = Bitset.get other.#Tf_pair.t on_var in
  let other_neg = Bitset.get other.#f on_var in
  (t_pos && other_neg) || (t_neg && other_pos)
;;

let of_int_array arr =
  let t = create_for_vec () in
  Array.iter arr ~f:(fun i ->
    let v = Int.abs i in
    match Ordering.of_int (Int.compare i 0) with
    | Equal -> failwith "invalid (0) int in arr"
    | Greater -> Bitset.set t.#t v
    | Less -> Bitset.set t.#f v);
  t
;;

let to_int_array t =
  let v = Vec.Value.create () in
  iter_literals t ~f:(fun literal -> Vec.Value.push v (Literal.to_int literal));
  Vec.Value.to_array v
;;

let resolve_exn t ~other ~on_var =
  if not (can_resolve t ~other ~on_var)
  then
    Error.raise_s
      [%message
        "Can't resolve clauses"
          (on_var : int)
          ~t:(to_int_array t : int array)
          ~other:(to_int_array other : int array)]
  else (
    Bitset.lor_inplace ~dest:t.#Tf_pair.t t.#t other.#Tf_pair.t;
    Bitset.lor_inplace ~dest:t.#f t.#f other.#f;
    Bitset.clear t.#t on_var;
    Bitset.clear t.#f on_var)
;;

module Pool = Pool.Make [@kind value & value] (T)
