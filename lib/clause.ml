open! Core
open! Import

type t =
  { lits : int Vec.Value.t
  ; mutable activity : F64.t
  ; mutable lbd : int
  ; mutable learnt : bool
  ; mutable deleted : bool
  ; mutable generation : int
  ; mutable watch0_pos : int
  ; mutable watch1_pos : int
  ; mutable watch0_slot : int
  ; mutable watch1_slot : int
  }

let sexp_of_t t = [%sexp_of: int list] (Vec.Value.to_list t.lits)

let create_for_pool () =
  { lits = Vec.Value.create ()
  ; activity = #0.
  ; lbd = 0
  ; learnt = false
  ; deleted = false
  ; generation = 0
  ; watch0_pos = -1
  ; watch1_pos = -1
  ; watch0_slot = -1
  ; watch1_slot = -1
  }
;;

include functor Poolable.Make [@kind value]

let activity t = t.activity
let set_activity t activity = t.activity <- activity
let lbd t = t.lbd
let set_lbd t lbd = t.lbd <- lbd
let learnt t = t.learnt
let set_learnt t learnt = t.learnt <- learnt
let deleted t = t.deleted
let set_deleted t deleted = t.deleted <- deleted
let generation t = t.generation

let bump_generation t =
  t.generation <- t.generation + 1;
  t.generation
;;

let watch_pos t ~watch = if watch = 0 then t.watch0_pos else t.watch1_pos
let watch_slot t ~watch = if watch = 0 then t.watch0_slot else t.watch1_slot

let set_watch_pos t ~watch pos =
  if watch = 0 then t.watch0_pos <- pos else t.watch1_pos <- pos
;;

let set_watch_slot t ~watch slot =
  if watch = 0 then t.watch0_slot <- slot else t.watch1_slot <- slot
;;

let clear_watch_data t =
  t.watch0_pos <- -1;
  t.watch1_pos <- -1;
  t.watch0_slot <- -1;
  t.watch1_slot <- -1
;;

let length t = Vec.Value.length t.lits
let get t i = Vec.Value.get t.lits i
let set t i v = Vec.Value.set t.lits i v
let negate t = Vec.Value.map_inplace t.lits ~f:Int.neg

let copy t =
  { lits = Vec.Value.copy t.lits
  ; activity = t.activity
  ; lbd = t.lbd
  ; learnt = t.learnt
  ; deleted = t.deleted
  ; generation = t.generation
  ; watch0_pos = t.watch0_pos
  ; watch1_pos = t.watch1_pos
  ; watch0_slot = t.watch0_slot
  ; watch1_slot = t.watch1_slot
  }
;;

let is_tautology t =
  let open Local_ref.O in
  let seen_taut = Local_ref.create false in
  for i = 1 to length t - 1 do
    if get t i = -get t (i - 1) then seen_taut := true
  done;
  !seen_taut [@nontail]
;;

let literal_is_true assignments literal =
  Bitset.get (Tf_pair.get assignments (literal > 0)) (Int.abs literal)
;;

let literal_is_false assignments literal =
  Bitset.get (Tf_pair.get assignments (literal < 0)) (Int.abs literal)
;;

let is_satisfied t ~assignments =
  let satisfied = ref false in
  let i = ref 0 in
  while !i < length t && not !satisfied do
    satisfied := literal_is_true assignments (get t !i);
    incr i
  done;
  !satisfied
;;

let iter_literals t ~(local_ f) =
  Vec.Value.iter t.lits ~f:(fun x -> f (Literal.of_int x)) [@nontail]
;;

let iteri t ~(local_ f) =
  Vec.Value.iteri t.lits ~f:(fun i x -> f i (Literal.of_int x)) [@nontail]
;;

let%template literals_list (t : t) : int list @ m =
  let rec go i acc =
    if i = length t then acc else go (i + 1) (get t i :: acc)
  in
  let res = go 0 [] in
  (List.rev [@alloc a]) res [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let contains t ~var =
  let open Local_ref.O in
  let seen = Local_ref.create false in
  Vec.Value.iter t.lits ~f:(fun literal ->
    if Int.abs literal = var then seen := true);
  !seen [@nontail]
;;

let sort_compare a b =
  if Int.abs a = Int.abs b then Int.compare a b else Int.compare (Int.abs a) (Int.abs b)
;;

let contains_literal t ~literal =
  let literal = Literal.to_int literal in
  match
    Vec.Value.binary_search
      t.lits
      ~f:(fun literal' -> sort_compare literal' literal)
      ~which:`First_equal
  with
  | None -> false
  | Some _ -> true
;;

let unit_literal t ~assignments =
  let candidate = ref None in
  let satisfied = ref false in
  let many_unset = ref false in
  let i = ref 0 in
  while !i < length t && not !satisfied && not !many_unset do
    let literal = get t !i in
    if literal_is_true assignments literal
    then satisfied := true
    else if not (literal_is_false assignments literal)
    then (
      match !candidate with
      | None -> candidate := Some literal
      | Some _ -> many_unset := true);
    incr i
  done;
  if !satisfied || !many_unset
  then Literal.Option.none ()
  else (
    match !candidate with
    | None -> Literal.Option.none ()
    | Some literal -> Literal.Option.some (Literal.of_int literal))
;;

type watched_clause_update =
  | Satisfied
  | Replacement of int
  | Unit of Literal.t
  | Conflict

let analyze_false_watch t ~assignments ~false_watch_pos ~other_watch_pos =
  let other_literal_is_true =
    other_watch_pos >= 0 && literal_is_true assignments (get t other_watch_pos)
  in
  if other_literal_is_true
  then Satisfied
  else (
    let replacement = ref (-1) in
    let i = ref 0 in
    while !i < length t && !replacement < 0 do
      if !i <> false_watch_pos && !i <> other_watch_pos
      then (
        let literal = get t !i in
        if not (literal_is_false assignments literal) then replacement := !i);
      incr i
    done;
    if !replacement >= 0
    then Replacement !replacement
    else if other_watch_pos < 0
    then Conflict
    else (
      let other_literal = get t other_watch_pos in
      if literal_is_false assignments other_literal
      then Conflict
      else Unit (Literal.of_int other_literal)))
;;

let clear t =
  Vec.Value.clear t.lits;
  t.activity <- #0.;
  t.lbd <- 0;
  t.learnt <- false;
  t.deleted <- true;
  clear_watch_data t
;;

let can_resolve t ~other ~on_var =
  let open Local_ref.O in
  let t_v = Tf_pair.create_local (fun _ -> exclave_ Local_ref.create false) in
  let other_v =
    Tf_pair.create_local (fun _ -> exclave_ Local_ref.create false)
  in
  let update clause v = exclave_
    iter_literals clause ~f:(fun lit ->
      if Literal.var lit = on_var
      then (
        let r = (Tf_pair.get [@mode local]) v (Literal.value lit) in
        (r := true) [@nontail]))
  in
  update t t_v;
  update other other_v;
  let try_ v = exclave_
    let t_r = (Tf_pair.get [@mode local]) t_v v in
    let other_r = (Tf_pair.get [@mode local]) other_v (not v) in
    !t_r && !other_r
  in
  try_ true || try_ false [@nontail]
;;

let of_int_array ?(lbd = 0) ?(learnt = false) arr =
  Array.sort arr ~compare:sort_compare;
  { lits = Vec.Value.of_array_taking_ownership arr
  ; activity = #0.
  ; lbd
  ; learnt
  ; deleted = false
  ; generation = 0
  ; watch0_pos = -1
  ; watch1_pos = -1
  ; watch0_slot = -1
  ; watch1_slot = -1
  }
;;

let t_of_sexp sexp =
  [%of_sexp: int list] sexp
  |> Array.of_list
  |> of_int_array
;;

let to_int_array t = Vec.Value.to_array t.lits

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
    let old_len = length t in
    Vec.Value.iter other.lits ~f:(fun x ->
      if Int.abs x <> on_var
      then
        match
          Vec.Value.binary_search
            ~end_:old_len
            t.lits
            ~f:(fun y -> sort_compare y x)
            ~which:`First_equal
        with
        | Some _ -> ()
        | None -> Vec.Value.push t.lits x);
    Vec.Value.sort_partitioned t.lits ~a_len:old_len ~compare:sort_compare;
    Vec.Value.filter_inplace t.lits ~f:(fun x -> Int.abs x <> on_var))
;;

let%expect_test "unit literal detects single remaining negative" =
  let clause = of_int_array [| -1; -4; -6 |] in
  let assignments = Tf_pair.create (fun (_ : bool) -> Bitset.create ()) in
  Bitset.set (Tf_pair.get assignments true) 4;
  Bitset.set (Tf_pair.get assignments true) 6;
  (match%optional_u (unit_literal clause ~assignments : Literal.Option.t) with
   | None -> print_endline "None"
   | Some literal -> Literal.to_int literal |> Int.to_string |> print_endline);
  [%expect {| -1 |}]
;;
