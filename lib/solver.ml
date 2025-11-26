open! Core
open! Import

module Sat_result = struct
  type t =
    | Sat of { assignments : Clause.t }
    | Unsat of { global_ unsat_core : Clause.t }
end

module Reason : sig
  type (_ : value mod external_, _ : bits64) tag : value mod external_ =
    | Decision : (_, Literal.t) tag
    | Clause_idx : (int, _) tag

  type t : (value & value & bits64) mod external_ = private
    | T : #(('a, 'b) tag * 'a * 'b) -> t
  [@@unboxed]
  (* Safe as long as noone puts non external_ values in the slot used for [Clause_idx].

     This is enforced in the interface, because [t] can't be constructed outside of the given constructors.
  *)
  [@@unsafe_allow_any_mode_crossing]

  val decision : Literal.t -> t
  val clause_idx : int -> t
end = struct
  type (_ : value mod external_, _ : bits64) tag : value mod external_ =
    | Decision : (_, Literal.t) tag
    | Clause_idx : (int, _) tag

  type t : (value & value & bits64) mod external_ =
    | T : #(('a, 'b) tag * 'a * 'b) -> t
  [@@unboxed] [@@unsafe_allow_any_mode_crossing]

  let decision literal = T #(Decision, 0, literal)
  let clause_idx idx = T #(Clause_idx, idx, Literal.of_int 0)
end

module Trail_entry = struct
  module T = struct
    type t : ((value & value & bits64) & bits64 & bits64) mod external_ =
      #{ reason : Reason.t
       ; literal : Literal.t
       ; decision_level : int64#
       }
    (* reason is actually [external_], idk why I need this *)
    [@@unsafe_allow_any_mode_crossing]

    let create_for_vec () =
      #{ literal = Literal.create ~var:0 ~value:true
       ; decision_level = #0L
       ; reason = Reason.clause_idx 0
       }
    ;;
  end

  include T
  module Vec = Vec.Make [@kind (value & value & bits64) & bits64 & bits64] (T)
end

type t =
  { debug : bool
  ; mutable has_empty_clause : bool
  ; mutable decision_level : int64#
  ; mutable iterations : int
  ; mutable clause_adjusting_score : Adjusting_score.t
  ; assignments : Bitset.t Tf_pair.t
  ; trail : Trail_entry.Vec.t
  ; clauses : Clause.Pool.t
  ; clause_scores : F64.Option.Vec.t
  ; clauses_with_active_unit : Bitset.t
  ; unprocessed_unit_clauses : Bitset.t
  ; clauses_by_literal : Bitset.t Vec.Value.t Tf_pair.t
  ; trail_entry_idx_by_var : I64.Option.Vec.t
  ; watched_clauses_by_literal : Bitset.t Vec.Value.t Tf_pair.t
  ; vsids : Vsids.t
  ; learned_clauses : Bitset.t
  ; simplify_clauses_every : int
  ; clause_sorting_buckets : int Vec.Value.t
  ; luby : Luby.t
  ; mutable conflicts : int64#
  }

let assignment t ~var = exclave_
  if Bitset.get (Tf_pair.get t.assignments true) var
  then Some true
  else if Bitset.get (Tf_pair.get t.assignments false) var
  then Some false
  else None
;;

let add_clause_activity t ~clause_idx =
  let inc = t.clause_adjusting_score.#inc in
  let to_set =
    match%optional_u
      (F64.Option.Vec.get t.clause_scores clause_idx : F64.Option.t)
    with
    | None -> inc
    | Some x -> F64.O.(inc + x)
  in
  F64.Option.Vec.set t.clause_scores clause_idx (F64.Option.some to_set);
  exclave_ F64.O.(to_set > t.clause_adjusting_score.#rescale)
;;

let rescale_clause_activities t =
  F64.Option.Vec.iteri t.clause_scores ~f:(fun i score ->
    match%optional_u.F64.Option score with
    | None -> ()
    | Some score ->
      F64.Option.Vec.set
        t.clause_scores
        i
        F64.O.(score / t.clause_adjusting_score.#rescale |> F64.Option.some));
  t.clause_adjusting_score <- Adjusting_score.rescale t.clause_adjusting_score
;;

let decay_clause_activities t =
  t.clause_adjusting_score <- Adjusting_score.decay t.clause_adjusting_score
;;

let learn_clause_from_failure ~failed_clause t =
  let learned = Clause.copy failed_clause in
  let num_at_level = ref 0 in
  if t.debug
  then
    print_s
      [%message
        "learn_clause_from_failure"
          ~learned:(Clause.to_int_array learned : int array)];
  Clause.iter_literals learned ~f:(fun literal ->
    match%optional_u
      (I64.Option.Vec.get t.trail_entry_idx_by_var (Literal.var literal)
       : I64.Option.t)
    with
    | None -> ()
    | Some idx ->
      let trail_entry = Trail_entry.Vec.get t.trail (I64.to_int_trunc idx) in
      if I64.O.(trail_entry.#decision_level = t.decision_level)
      then incr num_at_level);
  let rescale = Local_ref.create false in
  Trail_entry.Vec.iter_rev t.trail ~f:(fun trail_entry ->
    match
      #( !num_at_level = 1
         || not
              (Clause.contains learned ~var:(Literal.var trail_entry.#literal))
       , trail_entry.#reason )
    with
    | #(true, _) -> ()
    | #(false, T #(Decision, _, _)) ->
      failwith "found decision walking back from conflict"
    | #(false, T #(Clause_idx, clause_idx, _)) ->
      Vsids.add_activity t.vsids ~literal:trail_entry.#literal;
      Local_ref.O.(rescale := !rescale || add_clause_activity t ~clause_idx);
      let clause = Clause.Pool.get t.clauses (Ptr.of_int clause_idx) in
      if t.debug
      then
        print_s
          [%message
            "learn_clause_from_failure"
              ~learned_clause:(Clause.to_int_array learned : int array)
              ~see:(Clause.to_int_array clause : int array)
              (trail_entry.#literal : Literal.t)
              (!num_at_level : int)];
      Clause.iter_literals clause ~f:(fun literal ->
        let var = Literal.var literal in
        if Literal.equal literal trail_entry.#literal
        then decr num_at_level
        else if Clause.contains_literal learned ~literal
        then ()
        else (
          match%optional_u
            (I64.Option.Vec.get t.trail_entry_idx_by_var var : I64.Option.t)
          with
          | None -> ()
          | Some idx ->
            let trail_entry' =
              Trail_entry.Vec.get t.trail (I64.to_int_trunc idx)
            in
            if I64.O.(trail_entry'.#decision_level = t.decision_level)
            then incr num_at_level));
      if t.debug
      then
        print_s
          [%message
            "learn_clause_from_failure"
              ~learned_clause:(Clause.to_int_array learned : int array)
              ~after_see:(Clause.to_int_array clause : int array)
              (trail_entry.#literal : Literal.t)
              (!num_at_level : int)];
      Clause.resolve_exn
        learned
        ~other:clause
        ~on_var:(Literal.var trail_entry.#literal));
  if Local_ref.get rescale then rescale_clause_activities t;
  learned
;;

(** can be called multiple times for the same var *)
let on_new_var
  { clauses_by_literal
  ; trail_entry_idx_by_var
  ; watched_clauses_by_literal
  ; vsids
  ; assignments = _
  ; iterations = _
  ; decision_level = _
  ; trail = _
  ; clauses = _
  ; unprocessed_unit_clauses = _
  ; has_empty_clause = _
  ; debug = _
  ; clauses_with_active_unit = _
  ; clause_scores = _
  ; clause_adjusting_score = _
  ; learned_clauses = _
  ; simplify_clauses_every = _
  ; clause_sorting_buckets = _
  ; luby = _
  ; conflicts = _
  }
  ~var
  =
  Vsids.on_new_var vsids ~var;
  I64.Option.Vec.fill_to_length
    trail_entry_idx_by_var
    ~f:(fun (_ : int) -> I64.Option.none ())
    ~length:(var + 1);
  let fill tf = exclave_
    Tf_pair.iter
      tf
      ~f:
        (Vec.Value.fill_to_length ~length:(var + 1) ~f:(fun (_ : int) ->
           Bitset.create ()))
  in
  fill watched_clauses_by_literal;
  fill clauses_by_literal [@nontail]
;;

let get_by_literal by_literal literal =
  Vec.Value.get
    (Tf_pair.get by_literal (Literal.value literal))
    (Literal.var literal)
;;

(* return is [This clause_idx] for unsat [clause_idx] *)
let%template update_watched_clauses t ~set_literal =
  let literal = Literal.negate set_literal in
  let watched_clauses =
    Vec.Value.get
      (Tf_pair.get t.watched_clauses_by_literal (Literal.value literal))
      (Literal.var literal)
  in
  Bitset.fold_set_bits_or_null
    watched_clauses
    ~init:Null
    ~f:(fun ~done_ acc clause_idx ->
      match acc with
      | This _ ->
        Local_ref.set done_ true;
        acc
      | Null ->
        let clause = Clause.Pool.get t.clauses (Ptr.of_int clause_idx) in
        if Clause.is_satisfied clause ~assignments:t.assignments
        then Null
        else (
          if t.debug
          then
            print_s
              [%message
                "update_watched_clauses"
                  ~clause:(Clause.to_int_array clause : int array)];
          let replace_with =
            Clause.literals_list clause
            |> List.find_local ~f:(fun literal -> exclave_
              let literal' = Literal.of_int literal in
              match
                ( Bitset.get
                    (get_by_literal t.watched_clauses_by_literal literal')
                    clause_idx
                , assignment t ~var:(Literal.var literal') )
              with
              | true, _ | false, Some _ -> None
              | false, None -> Some literal)
          in
          match replace_with with
          | Some to_replace ->
            Bitset.clear watched_clauses clause_idx;
            Bitset.clear
              (get_by_literal t.watched_clauses_by_literal literal)
              clause_idx;
            Bitset.set
              (get_by_literal
                 t.watched_clauses_by_literal
                 (Literal.of_int to_replace))
              clause_idx;
            Null
          | None ->
            (match%optional_u
               (Clause.unit_literal clause ~assignments:t.assignments
                : Literal.Option.t)
             with
             | None -> This clause_idx
             | Some (_ : Literal.t) ->
               Bitset.set t.unprocessed_unit_clauses clause_idx;
               Null)))
;;

let populate_watched_literals_for_new_clause
  ({ clauses
   ; watched_clauses_by_literal
   ; unprocessed_unit_clauses
   ; clauses_by_literal = _
   ; assignments = _
   ; iterations = _
   ; decision_level = _
   ; trail = _
   ; trail_entry_idx_by_var = _
   ; vsids = _
   ; has_empty_clause = _
   ; debug = _
   ; clauses_with_active_unit = _
   ; clause_scores = _
   ; clause_adjusting_score = _
   ; learned_clauses = _
   ; simplify_clauses_every = _
   ; clause_sorting_buckets = _
   ; luby = _
   ; conflicts = _
   } as t)
  ~ptr
  =
  let satisfied = ref false in
  let clause = Clause.Pool.get clauses ptr in
  let unset_literals, set_literals =
    List.partition_tf_local (Clause.literals_list clause) ~f:(fun l ->
      let literal = Literal.of_int l in
      let value = Literal.value literal in
      match assignment t ~var:(Literal.var literal) with
      | None -> true
      | Some b when Bool.(b = value) ->
        satisfied := true;
        false
      | Some _ -> false)
  in
  let maybe_unit () =
    if !satisfied
    then ()
    else Bitset.set unprocessed_unit_clauses (Ptr.to_int ptr)
  in
  let watch_literal (literal : int) =
    let bs =
      Vec.Value.get
        (Tf_pair.get watched_clauses_by_literal (literal > 0))
        (Int.abs literal)
    in
    Bitset.set bs (Ptr.to_int ptr)
  in
  match #(unset_literals, set_literals) with
  | #([], a :: b :: _) ->
    watch_literal a;
    watch_literal b
  | #([ a ], b :: _) ->
    maybe_unit ();
    watch_literal a;
    watch_literal b
  | #([ a ], []) ->
    maybe_unit ();
    watch_literal a
  | #(a :: b :: _, _) ->
    watch_literal a;
    watch_literal b
  | #([], [ b ]) -> watch_literal b
  | #([], []) -> t.has_empty_clause <- true
;;

(** can NOT be called multiple times for the same clause *)
let push_clause
  ({ clauses
   ; clauses_by_literal
   ; clause_scores
   ; clause_adjusting_score
   ; watched_clauses_by_literal = _
   ; iterations = _
   ; unprocessed_unit_clauses = _
   ; assignments = _
   ; decision_level = _
   ; trail = _
   ; trail_entry_idx_by_var = _
   ; vsids = _
   ; has_empty_clause = _
   ; debug = _
   ; clauses_with_active_unit = _
   ; learned_clauses = _
   ; simplify_clauses_every = _
   ; clause_sorting_buckets = _
   ; luby = _
   ; conflicts = _
   } as t)
  ~clause
  =
  let ptr = Clause.Pool.alloc clauses in
  Clause.Pool.set clauses ptr clause;
  (* bookkeeping for vars *)
  Clause.iter_literals clause ~f:(fun literal ->
    let var = Literal.var literal in
    on_new_var t ~var;
    let clauses_for_lit =
      Vec.Value.get (Tf_pair.get clauses_by_literal (Literal.value literal)) var
    in
    Bitset.set clauses_for_lit (Ptr.to_int ptr));
  F64.Option.Vec.push
    clause_scores
    (F64.Option.some (Adjusting_score.unit clause_adjusting_score));
  populate_watched_literals_for_new_clause t ~ptr;
  ptr
;;

let free_clause
  ({ clauses
   ; clauses_by_literal
   ; watched_clauses_by_literal
   ; unprocessed_unit_clauses
   ; clauses_with_active_unit
   ; clause_scores
   ; clause_adjusting_score = _
   ; assignments = _
   ; decision_level = _
   ; trail = _
   ; iterations = _
   ; trail_entry_idx_by_var = _
   ; vsids = _
   ; has_empty_clause = _
   ; debug = _
   ; learned_clauses = _
   ; simplify_clauses_every = _
   ; clause_sorting_buckets = _
   ; luby = _
   ; conflicts = _
   } as t)
  ptr
  =
  let clause_idx = Ptr.to_int ptr in
  let clause = Clause.Pool.get clauses ptr in
  Clause.iter_literals clause ~f:(fun literal ->
    let var = Literal.var literal in
    on_new_var t ~var;
    let clear_bs bs_pair =
      let clauses_for_lit =
        Vec.Value.get (Tf_pair.get bs_pair (Literal.value literal)) var
      in
      Bitset.clear clauses_for_lit (Ptr.to_int ptr)
    in
    clear_bs clauses_by_literal;
    clear_bs watched_clauses_by_literal);
  Bitset.clear unprocessed_unit_clauses clause_idx;
  Bitset.clear clauses_with_active_unit clause_idx;
  F64.Option.Vec.set clause_scores clause_idx (F64.Option.none ());
  Clause.clear clause;
  Clause.Pool.free t.clauses ptr
;;

let add_to_trail t ~(trail_entry : Trail_entry.t) =
  if t.debug
  then
    print_s
      [%message
        "add_to_trail"
          (trail_entry.#literal : Literal.t)
          (t.decision_level : I64.t)];
  assert (
    not
      (Bitset.get
         (Tf_pair.get t.assignments true)
         (Literal.var trail_entry.#literal)
       || Bitset.get
            (Tf_pair.get t.assignments false)
            (Literal.var trail_entry.#literal)));
  (match trail_entry.#reason with
   | T #(Decision, _, _) -> ()
   | T #(Clause_idx, clause_ptr, _) ->
     Bitset.set t.clauses_with_active_unit clause_ptr);
  Bitset.set
    (Tf_pair.get t.assignments (Literal.value trail_entry.#literal))
    (Literal.var trail_entry.#literal);
  I64.Option.Vec.set
    t.trail_entry_idx_by_var
    (Literal.var trail_entry.#literal)
    (Trail_entry.Vec.length t.trail |> I64.of_int |> I64.Option.some);
  Vsids.remove_from_pool t.vsids ~var:(Literal.var trail_entry.#literal);
  Trail_entry.Vec.push t.trail trail_entry;
  update_watched_clauses t ~set_literal:trail_entry.#literal
;;

let undo_entry t ~(trail_entry : Trail_entry.t) =
  if t.debug
  then
    print_s
      [%message
        "undo_entry"
          (trail_entry.#literal : Literal.t)
          (t.decision_level : I64.t)];
  assert (
    Bitset.get
      (Tf_pair.get t.assignments (Literal.value trail_entry.#literal))
      (Literal.var trail_entry.#literal));
  (match trail_entry.#reason with
   | T #(Decision, _, _) -> ()
   | T #(Clause_idx, clause_ptr, _) ->
     Bitset.clear t.clauses_with_active_unit clause_ptr);
  Vsids.add_to_pool t.vsids ~var:(Literal.var trail_entry.#literal);
  Bitset.clear
    (Tf_pair.get t.assignments (Literal.value trail_entry.#literal))
    (Literal.var trail_entry.#literal);
  I64.Option.Vec.set
    t.trail_entry_idx_by_var
    (Literal.var trail_entry.#literal)
    (I64.Option.none ())
;;

let rec remove_greater_than_decision_level t ~decision_level =
  let remaining_level =
    if Trail_entry.Vec.length t.trail = 0
    then #0L
    else (Trail_entry.Vec.last_exn t.trail).#decision_level
  in
  if I64.O.(remaining_level <= decision_level)
  then t.decision_level <- remaining_level
  else (
    let trail_entry = Trail_entry.Vec.pop_exn t.trail in
    undo_entry t ~trail_entry;
    remove_greater_than_decision_level t ~decision_level)
;;

let second_highest_decision_level t ~clause =
  let #(_, max2) =
    Clause.literals_list clause
    |> (List.fold_local [@kind bits64 & bits64])
         ~init:#(#0L, #0L)
         ~f:(fun (#(max1, max2) as acc) literal -> exclave_
           match%optional_u
             (I64.Option.Vec.get t.trail_entry_idx_by_var (Int.abs literal)
              : I64.Option.t)
           with
           | None -> acc
           | Some idx ->
             let trail_entry =
               Trail_entry.Vec.get t.trail (I64.to_int_trunc idx)
             in
             let dl = trail_entry.#decision_level in
             if I64.O.(dl >= max1)
             then #(dl, max1)
             else if I64.O.(dl >= max2)
             then #(max1, dl)
             else acc)
  in
  (* if no snd highest, we want to return 0, which naturally happens by initing
     to #0L *)
  max2
;;

let backtrack t ~failed_clause =
  let learned_clause = learn_clause_from_failure t ~failed_clause in
  if t.debug
  then
    print_s
      [%message
        "backtrack"
          ~learned_clause:(Clause.literals_list learned_clause : int list)];
  Clause.iter_literals learned_clause ~f:(fun literal ->
    Vsids.add_activity t.vsids ~literal);
  Vsids.decay t.vsids;
  (* This is correct because we backtrack to the previous decision level, where all unit clauses had been applied. Learned clause is set to unit after adding here. *)
  Bitset.clear_all t.unprocessed_unit_clauses;
  remove_greater_than_decision_level
    t
    ~decision_level:(second_highest_decision_level t ~clause:learned_clause);
  let ptr = push_clause t ~clause:learned_clause in
  Bitset.set t.learned_clauses (Ptr.to_int ptr)
;;

let%template make_decision t : _ @ m =
  match%optional_u (Vsids.choose_literal t.vsids : Literal.Option.t) with
  | None ->
    let literals =
      Array.append
        (Bitset.to_set_bits_array (Tf_pair.get t.assignments true))
        (Bitset.to_set_bits_array (Tf_pair.get t.assignments false)
         |> Array.map ~f:Int.neg)
    in
    `Done (Sat_result.Sat { assignments = Clause.of_int_array literals })
  | Some literal ->
    if t.debug then print_s [%message "make_decision" (literal : Literal.t)];
    t.decision_level <- I64.O.(t.decision_level + #1L);
    let trail_entry : Trail_entry.t =
      #{ reason = Reason.decision literal
       ; literal
       ; decision_level = t.decision_level
       }
    in
    let (_ : int or_null) = add_to_trail t ~trail_entry in
    `Continue
[@@exclave_if_stack] [@@alloc a @ m = (stack_local, heap_global)]
;;

let restart t =
  t.conflicts <- #0L;
  t.decision_level <- #0L;
  Bitset.clear_all t.unprocessed_unit_clauses;
  while Trail_entry.Vec.length t.trail <> 0 do
    undo_entry t ~trail_entry:(Trail_entry.Vec.pop_exn t.trail)
  done;
  Clause.Pool.iter t.clauses ~f:(fun ptr ->
    let clause = Clause.Pool.get t.clauses ptr in
    match%optional_u
      (Clause.unit_literal clause ~assignments:t.assignments : Literal.Option.t)
    with
    | None -> ()
    | Some _ -> Bitset.set t.unprocessed_unit_clauses (Ptr.to_int ptr))
;;

let%template check_sat_result t ~(sat_result : _ @ m) : _ @ m =
  match (sat_result : Sat_result.t) with
  | Unsat _ -> sat_result
  | Sat _ ->
    if t.debug
    then
      Clause.Pool.iter t.clauses ~f:(fun ptr ->
        let clause = Clause.Pool.get t.clauses ptr in
        assert (Clause.is_satisfied clause ~assignments:t.assignments));
    sat_result
[@@alloc a @ m = (stack_local, heap_global)]
;;

let%template can_trim_clause t ~clause_idx =
  let clause = Clause.Pool.get t.clauses (Ptr.of_int clause_idx) in
  let literals = (Clause.literals_list [@alloc stack]) clause in
  let enough_literals =
    match literals with
    | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] -> false
    | _ -> true
  in
  let rec enough_lbd (local_ distinct_decision_levels) remaining_literals
    = exclave_
    match remaining_literals with
    | [] -> List.length_local distinct_decision_levels >= 4
    | literal :: remaining_literals ->
      (match%optional_u.I64.Option
         I64.Option.Vec.get t.trail_entry_idx_by_var (Int.abs literal)
       with
       | None -> enough_lbd distinct_decision_levels remaining_literals
       | Some idx ->
         let dl =
           (Trail_entry.Vec.get t.trail (I64.to_int_trunc idx)).#decision_level
         in
         (match
            List.find_local distinct_decision_levels ~f:(fun dl' -> exclave_
              if I64.(dl = of_int dl') then Some () else None)
          with
          | Some () -> enough_lbd distinct_decision_levels remaining_literals
          | None ->
            let distinct_decision_levels =
              I64.to_int_trunc dl :: distinct_decision_levels
            in
            if List.length_local distinct_decision_levels >= 4
            then true
            else enough_lbd distinct_decision_levels remaining_literals))
  in
  let enough_lbd = enough_lbd [] literals in
  (not (Bitset.get t.clauses_with_active_unit clause_idx))
  && enough_literals
  && enough_lbd
;;

let simplify_clauses t =
  Vec.Value.clear t.clause_sorting_buckets;
  Clause.Pool.iter t.clauses ~f:(fun ptr ->
    let clause_idx = Ptr.to_int ptr in
    if Bitset.get t.learned_clauses clause_idx
       && (not (Bitset.get t.clauses_with_active_unit clause_idx))
       && can_trim_clause t ~clause_idx
    then Vec.Value.push t.clause_sorting_buckets clause_idx);
  let compare_by_score a b =
    F64.Option.compare
      (F64.Option.Vec.get t.clause_scores a)
      (F64.Option.Vec.get t.clause_scores b)
  in
  Vec.Value.sort t.clause_sorting_buckets ~compare:compare_by_score;
  let num_to_drop = Vec.Value.length t.clause_sorting_buckets / 2 in
  for i = 0 to num_to_drop - 1 do
    let clause_idx = Vec.Value.get t.clause_sorting_buckets i in
    if t.debug
    then (
      let clause = Clause.Pool.get t.clauses (Ptr.of_int clause_idx) in
      print_s
        [%message
          "Deleting clause"
            (clause_idx : int)
            ~clause:(Clause.to_int_array clause : int array)]);
    free_clause t (Ptr.of_int clause_idx);
    Bitset.clear t.learned_clauses clause_idx
  done
;;

let%template rec solve' t : Sat_result.t @ m =
  (t.iterations <- t.iterations + 1;
   if t.iterations mod t.simplify_clauses_every = 0
   then (
     if t.debug
     then
       print_s
         [%message
           "simplifying clauses"
             (t.iterations : int)
             ~num_clauses:(Clause.Pool.outstanding t.clauses : int)
             (t.decision_level : I64.t)];
     simplify_clauses t;
     decay_clause_activities t);
   let rec try_unit_propagate () = exclave_
     match Bitset.find_first_set t.unprocessed_unit_clauses ~start_pos:0 with
     | Null -> None
     | This clause_idx ->
       Bitset.clear t.unprocessed_unit_clauses clause_idx;
       let clause = Clause.Pool.get t.clauses (Ptr.of_int clause_idx) in
       let literal = Clause.unit_literal clause ~assignments:t.assignments in
       (match%optional_u (literal : Literal.Option.t) with
        | None -> try_unit_propagate ()
        | Some literal ->
          if t.debug
          then
            print_s
              [%message
                "try_unit_propagate: found unit"
                  ~clause:(Clause.to_int_array clause : int array)
                  (literal : Literal.t)];
          (match
             add_to_trail
               t
               ~trail_entry:
                 #{ decision_level = t.decision_level
                  ; literal
                  ; reason = Reason.clause_idx clause_idx
                  }
           with
           | Null -> try_unit_propagate ()
           | This failed_clause_idx -> Some failed_clause_idx))
   in
   match try_unit_propagate () with
   | None ->
     (match (make_decision [@alloc a]) t with
      | `Continue -> solve' t
      | `Done sat_result -> (check_sat_result [@alloc a]) t ~sat_result)
   | Some failed_clause_idx when I64.O.(t.decision_level = #0L) ->
     let failed_clause =
       Clause.Pool.get t.clauses (Ptr.of_int failed_clause_idx)
     in
     let learned_clause =
       (* raise when it's conflicts from a unit clause we deduced. *)
       try learn_clause_from_failure t ~failed_clause with
       | _ -> Clause.copy failed_clause
     in
     Unsat { unsat_core = learned_clause }
   | Some failed_clause_idx ->
     let failed_clause =
       Clause.Pool.get t.clauses (Ptr.of_int failed_clause_idx)
     in
     backtrack t ~failed_clause;
     t.conflicts <- I64.O.(t.conflicts + #1L);
     if I64.O.(t.conflicts >= Luby.value t.luby && t.decision_level > #0L)
     then (
       ignore (Luby.next t.luby);
       restart t);
     solve' t)
  [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let%template solve t : Sat_result.t @ m =
  if t.iterations > 0
  then (
    t.iterations <- 0;
    restart t);
  if t.has_empty_clause
  then Unsat { unsat_core = Clause.of_int_array [||] }
  else (solve' [@alloc a]) t [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let create ?(debug = false) () =
  { has_empty_clause = false
  ; decision_level = #0L
  ; iterations = 0
  ; assignments = Tf_pair.create (fun (_ : bool) -> Bitset.create ())
  ; trail = Trail_entry.Vec.create ()
  ; clauses = Clause.Pool.create ~chunk_size:4096 ()
  ; unprocessed_unit_clauses = Bitset.create ()
  ; clauses_with_active_unit = Bitset.create ()
  ; clause_scores = F64.Option.Vec.create ()
  ; clause_adjusting_score = Adjusting_score.default ()
  ; clauses_by_literal = Tf_pair.create (fun (_ : bool) -> Vec.Value.create ())
  ; trail_entry_idx_by_var = I64.Option.Vec.create ()
  ; watched_clauses_by_literal =
      Tf_pair.create (fun (_ : bool) -> Vec.Value.create ())
  ; vsids = Vsids.create ()
  ; learned_clauses = Bitset.create ()
  ; simplify_clauses_every = 2500
  ; clause_sorting_buckets = Vec.Value.create ()
  ; luby = Luby.create ~unit_run:#32L
  ; conflicts = #0L
  ; debug
  }
;;

let add_clause t ~clause =
  ignore (push_clause t ~clause);
  t
;;

let add_clause' t ~clause = add_clause t ~clause:(Clause.of_int_array clause)
