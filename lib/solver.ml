open! Core
open! Import

module Sat_result = struct
  type t =
    | Sat of { assignments : Clause.t }
    | Unsat of { global_ unsat_core : Clause.t }
  [@@deriving sexp]
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

module Pending_unit = struct
  type t = int * int * int
end

type t =
  { debug : bool
  ; mutable has_empty_clause : bool
  ; mutable decision_level : int64#
  ; mutable decision_level_of_last_assumption : int64#
  ; mutable iterations : int
  ; mutable clause_adjusting_score : Adjusting_score.t
  ; assignments : Bitset.t Tf_pair.t
  ; trail : Trail_entry.Vec.t
  ; clauses : Clause.Pool.t
  ; clauses_with_active_unit : Int.H_set.t
  ; pending_units : Pending_unit.t Vec.Value.t
  ; clauses_by_literal : Int.H_set.t Vec.Value.t Tf_pair.t
  ; trail_entry_idx_by_var : I64.Option.Vec.t
  ; watched_clauses_by_literal : (int * int) Vec.Value.t Vec.Value.t Tf_pair.t
  ; vsids : Vsids.t
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

let%template assignments_array t : _ @ m =
  (let open Local_ref.O in
   let f = Tf_pair.get t.assignments false in
   let t = Tf_pair.get t.assignments true in
   let len = Bitset.popcount f + Bitset.popcount t in
   let res = (Array.create [@alloc a]) ~len 0 in
   let loc = Local_ref.create 0 in
   let add i =
     res.(!loc) <- i;
     loc := !loc + 1
   in
   Bitset.iter_set_bits t ~f:add;
   Bitset.iter_set_bits f ~f:(fun i -> add (-i));
   res) [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let _ = assignments_array [@alloc stack]

let add_clause_activity t ~clause_idx =
  let inc = t.clause_adjusting_score.#inc in
  let clause = Clause.Pool.get t.clauses (Ptr.of_int clause_idx) in
  let to_set = F64.O.(inc + Clause.activity clause) in
  Clause.set_activity clause to_set;
  exclave_ F64.O.(to_set > t.clause_adjusting_score.#rescale)
;;

let rescale_clause_activities t =
  Clause.Pool.iter t.clauses ~f:(fun ptr ->
    let clause = Clause.Pool.get t.clauses ptr in
    Clause.set_activity
      clause
      F64.O.(Clause.activity clause / t.clause_adjusting_score.#rescale));
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
      Clause.resolve_exn
        learned
        ~other:clause
        ~on_var:(Literal.var trail_entry.#literal);
      if t.debug
      then
        print_s
          [%message
            "learn_clause_from_failure"
              ~learned_clause:(Clause.to_int_array learned : int array)
              ~after_see:(Clause.to_int_array clause : int array)
              (trail_entry.#literal : Literal.t)
              (!num_at_level : int)]);
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
  ; decision_level_of_last_assumption = _
  ; trail = _
  ; clauses = _
  ; pending_units = _
  ; has_empty_clause = _
  ; debug = _
  ; clauses_with_active_unit = _
  ; clause_adjusting_score = _
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
  let fill tf with_ = exclave_
    Tf_pair.iter
      tf
      ~f:
        (Vec.Value.fill_to_length ~length:(var + 1) ~f:(fun (_ : int) ->
           with_ ()))
  in
  fill clauses_by_literal Int.H_set.create;
  fill watched_clauses_by_literal Vec.Value.create [@nontail]
;;

let get_by_literal by_literal literal =
  Vec.Value.get
    (Tf_pair.get by_literal (Literal.value literal))
    (Literal.var literal)
;;

let queue_pending_unit t ~clause_idx ~literal =
  let clause = Clause.Pool.get t.clauses (Ptr.of_int clause_idx) in
  let generation = Clause.generation clause in
  if Clause.pending_unit_generation clause <> generation
  then (
    Clause.set_pending_unit_generation clause generation;
    Vec.Value.push t.pending_units (clause_idx, generation, Literal.to_int literal))
;;

let clear_pending_units t =
  Vec.Value.iter t.pending_units ~f:(fun (clause_idx, generation, _) ->
    let clause = Clause.Pool.get t.clauses (Ptr.of_int clause_idx) in
    if Clause.pending_unit_generation clause = generation
    then Clause.set_pending_unit_generation clause (-1));
  Vec.Value.clear t.pending_units
;;

let remove_watcher_at t ~literal ~slot =
  let watched_clauses = get_by_literal t.watched_clauses_by_literal literal in
  let last_slot = Vec.Value.length watched_clauses - 1 in
  let moved_clause_idx, moved_watch = Vec.Value.get watched_clauses last_slot in
  if slot <> last_slot
  then (
    Vec.Value.set watched_clauses slot (moved_clause_idx, moved_watch);
    let moved_clause =
      Clause.Pool.get t.clauses (Ptr.of_int moved_clause_idx)
    in
    Clause.set_watch_slot moved_clause ~watch:moved_watch slot);
  ignore (Vec.Value.pop_exn watched_clauses)
;;

let add_watcher t ~clause_idx ~watch ~watch_pos =
  let clause = Clause.Pool.get t.clauses (Ptr.of_int clause_idx) in
  let literal = Literal.of_int (Clause.get clause watch_pos) in
  let watched_clauses = get_by_literal t.watched_clauses_by_literal literal in
  let slot = Vec.Value.length watched_clauses in
  Vec.Value.push watched_clauses (clause_idx, watch);
  Clause.set_watch_pos clause ~watch watch_pos;
  Clause.set_watch_slot clause ~watch slot
;;

let remove_clause_watches t clause =
  let remove watch =
    let pos = Clause.watch_pos clause ~watch in
    if pos >= 0
    then (
      let literal = Literal.of_int (Clause.get clause pos) in
      let slot = Clause.watch_slot clause ~watch in
      remove_watcher_at t ~literal ~slot)
  in
  let pos0 = Clause.watch_pos clause ~watch:0 in
  let pos1 = Clause.watch_pos clause ~watch:1 in
  if pos0 >= 0 && pos1 >= 0 && Clause.get clause pos0 = Clause.get clause pos1
  then
    if Clause.watch_slot clause ~watch:0 > Clause.watch_slot clause ~watch:1
    then (
      remove 0;
      remove 1)
    else (
      remove 1;
      remove 0)
  else (
    remove 1;
    remove 0);
  Clause.clear_watch_data clause
;;

(* return is [This clause_idx] for unsat [clause_idx] *)
let%template update_watched_clauses t ~set_literal =
  let literal = Literal.negate set_literal in
  let watched_clauses = get_by_literal t.watched_clauses_by_literal literal in
  let rec process idx =
    if idx >= Vec.Value.length watched_clauses
    then Null
    else (
      let clause_idx, watch = Vec.Value.get watched_clauses idx in
      let clause = Clause.Pool.get t.clauses (Ptr.of_int clause_idx) in
      let false_watch_pos = Clause.watch_pos clause ~watch in
      if Clause.watch_slot clause ~watch <> idx
         || false_watch_pos < 0
         || Clause.get clause false_watch_pos <> Literal.to_int literal
      then (
        remove_watcher_at t ~literal ~slot:idx;
        process idx)
      else (
        let other_watch_pos = Clause.watch_pos clause ~watch:(1 - watch) in
        match
          Clause.analyze_false_watch
            clause
            ~assignments:t.assignments
            ~false_watch_pos
            ~other_watch_pos
        with
        | Satisfied -> process (idx + 1)
        | Replacement replacement_pos ->
          remove_watcher_at t ~literal ~slot:idx;
          add_watcher t ~clause_idx ~watch ~watch_pos:replacement_pos;
          process idx
        | Unit unit_literal ->
          queue_pending_unit t ~clause_idx ~literal:unit_literal;
          process (idx + 1)
        | Conflict -> This clause_idx))
  in
  process 0 [@nontail]
;;

let populate_watched_literals_for_new_clause
  ({ clauses
   ; watched_clauses_by_literal = _
   ; pending_units = _
   ; clauses_by_literal = _
   ; assignments = _
   ; iterations = _
   ; decision_level_of_last_assumption = _
   ; decision_level = _
   ; trail = _
   ; trail_entry_idx_by_var = _
   ; vsids = _
   ; has_empty_clause = _
   ; debug = _
   ; clauses_with_active_unit = _
   ; clause_adjusting_score = _
   ; simplify_clauses_every = _
   ; clause_sorting_buckets = _
   ; luby = _
   ; conflicts = _
   } as t)
  ~ptr
  =
  let clause = Clause.Pool.get clauses ptr in
  let clause_idx = Ptr.to_int ptr in
  Clause.clear_watch_data clause;
  if Clause.length clause = 0
  then t.has_empty_clause <- true
  else (
    let first_non_false = ref (-1) in
    let second_non_false = ref (-1) in
    let first_fallback = ref (-1) in
    let satisfied = ref false in
    for i = 0 to Clause.length clause - 1 do
      let literal = Literal.of_int (Clause.get clause i) in
      match assignment t ~var:(Literal.var literal) with
      | Some value when Bool.equal value (Literal.value literal) ->
        satisfied := true;
        if !first_non_false < 0
        then first_non_false := i
        else if !second_non_false < 0
        then second_non_false := i
      | Some _ -> if !first_fallback < 0 then first_fallback := i
      | None ->
        if !first_non_false < 0
        then first_non_false := i
        else if !second_non_false < 0
        then second_non_false := i
    done;
    let watch0 = if !first_non_false >= 0 then !first_non_false else 0 in
    let watch1 =
      if !second_non_false >= 0
      then !second_non_false
      else if Clause.length clause > 1
      then (
        let fallback =
          if !first_fallback >= 0 && !first_fallback <> watch0
          then !first_fallback
          else if watch0 = 0
          then 1
          else 0
        in
        fallback)
      else -1
    in
    add_watcher t ~clause_idx ~watch:0 ~watch_pos:watch0;
    if watch1 >= 0 then add_watcher t ~clause_idx ~watch:1 ~watch_pos:watch1;
    if (not !satisfied) && !first_non_false >= 0 && !second_non_false < 0
    then
      queue_pending_unit
        t
        ~clause_idx
        ~literal:(Literal.of_int (Clause.get clause !first_non_false)))
;;

(** can NOT be called multiple times for the same clause *)
let push_clause
  ({ clauses
   ; clauses_by_literal
   ; clause_adjusting_score
   ; watched_clauses_by_literal = _
   ; iterations = _
   ; pending_units = _
   ; assignments = _
   ; decision_level_of_last_assumption = _
   ; decision_level = _
   ; trail = _
   ; trail_entry_idx_by_var = _
   ; vsids = _
   ; has_empty_clause = _
   ; debug = _
   ; clauses_with_active_unit = _
   ; simplify_clauses_every = _
   ; clause_sorting_buckets = _
   ; luby = _
   ; conflicts = _
   } as t)
  ~clause
  =
  let ptr = Clause.Pool.alloc clauses in
  Clause.Pool.set clauses ptr clause;
  let clause = Clause.Pool.get clauses ptr in
  Clause.set_deleted clause false;
  Clause.set_activity clause (Adjusting_score.unit clause_adjusting_score);
  ignore (Clause.bump_generation clause);
  (* bookkeeping for vars *)
  Clause.iter_literals clause ~f:(fun literal ->
    let var = Literal.var literal in
    on_new_var t ~var;
    let clauses_for_lit =
      Vec.Value.get (Tf_pair.get clauses_by_literal (Literal.value literal)) var
    in
    Int.H_set.insert clauses_for_lit ~key:(Ptr.to_int ptr) ~data:());
  populate_watched_literals_for_new_clause t ~ptr;
  ptr
;;

let free_clause
  ({ clauses
   ; clauses_by_literal
   ; watched_clauses_by_literal = _
   ; pending_units = _
   ; clauses_with_active_unit
   ; clause_adjusting_score = _
   ; assignments = _
   ; decision_level = _
   ; decision_level_of_last_assumption = _
   ; trail = _
   ; iterations = _
   ; trail_entry_idx_by_var = _
   ; vsids = _
   ; has_empty_clause = _
   ; debug = _
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
    let get_tang by_literal =
      Vec.Value.get (Tf_pair.get by_literal (Literal.value literal)) var
    in
    Int.H_set.remove (get_tang clauses_by_literal) (Ptr.to_int ptr));
  remove_clause_watches t clause;
  Int.H_set.remove clauses_with_active_unit clause_idx;
  Clause.set_deleted clause true;
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
     Int.H_set.insert t.clauses_with_active_unit ~key:clause_ptr ~data:());
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
     Int.H_set.remove t.clauses_with_active_unit clause_ptr);
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
    let rec go i (#(max1, max2) as acc) = exclave_
      if i = Clause.length clause
      then acc
      else (
        let literal = Clause.get clause i in
        let acc =
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
            else acc
        in
        go (i + 1) acc)
    in
    go 0 #(#0L, #0L)
  in
  (* if no snd highest, we want to return 0, which naturally happens by initing
     to #0L *)
  max2
;;

let clause_lbd t ~clause =
  let distinct = Int.H_set.create () in
  for i = 0 to Clause.length clause - 1 do
    let literal = Clause.get clause i in
    match%optional_u
      (I64.Option.Vec.get t.trail_entry_idx_by_var (Int.abs literal)
       : I64.Option.t)
    with
    | None -> ()
    | Some idx ->
      let dl =
        (Trail_entry.Vec.get t.trail (I64.to_int_trunc idx)).#decision_level
      in
      Int.H_set.insert distinct ~key:(I64.to_int_trunc dl) ~data:()
  done;
  Int.H_set.length distinct
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
  clear_pending_units t;
  remove_greater_than_decision_level
    t
    ~decision_level:(second_highest_decision_level t ~clause:learned_clause);
  Clause.set_learnt learned_clause true;
  Clause.set_lbd learned_clause (clause_lbd t ~clause:learned_clause);
  ignore (push_clause t ~clause:learned_clause)
;;

let%template make_decision' ~is_assumption t ~literal : _ @ m =
  if t.debug then print_s [%message "make_decision" (literal : Literal.t)];
  t.decision_level <- I64.O.(t.decision_level + #1L);
  if is_assumption then t.decision_level_of_last_assumption <- t.decision_level;
  let trail_entry : Trail_entry.t =
    #{ reason = Reason.decision literal
     ; literal
     ; decision_level = t.decision_level
     }
  in
  match add_to_trail t ~trail_entry with
  | Null -> `Continue
  | This i -> `Failed_clause i
[@@exclave_if_stack] [@@alloc a @ m = (stack_local, heap_global)]
;;

let%template make_decision t : _ @ m =
  match%optional_u (Vsids.choose_literal t.vsids : Literal.Option.t) with
  | None ->
    `Done
      (Sat_result.Sat
         { assignments = Clause.of_int_array (assignments_array t) })
  | Some literal ->
    (make_decision' [@alloc a])
      ~is_assumption:false
      t
      ~literal [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let%template check_sat_result t ~(sat_result : _ @ m) : _ @ m =
  match (sat_result : Sat_result.t) with
  | Unsat _ -> sat_result
  | Sat _ ->
    if t.debug
    then
      Clause.Pool.iter t.clauses ~f:(fun ptr ->
        let clause = Clause.Pool.get t.clauses ptr in
        if not (Clause.is_satisfied clause ~assignments:t.assignments)
        then
          Error.raise_s
            [%message
              "BUG: clause isn't satisfied in result"
                ~clause:(Clause.to_int_array clause : int array)]);
    sat_result
[@@alloc a @ m = (stack_local, heap_global)]
;;

let%template can_trim_clause t ~clause_idx =
  let clause = Clause.Pool.get t.clauses (Ptr.of_int clause_idx) in
  (not (Int.H_set.mem t.clauses_with_active_unit clause_idx))
  && Clause.length clause > 3
  && Clause.lbd clause >= 4
;;

let simplify_clauses t =
  Vec.Value.clear t.clause_sorting_buckets;
  Clause.Pool.iter t.clauses ~f:(fun ptr ->
    let clause_idx = Ptr.to_int ptr in
    let clause = Clause.Pool.get t.clauses ptr in
    if Clause.learnt clause
       && (not (Int.H_set.mem t.clauses_with_active_unit clause_idx))
       && can_trim_clause t ~clause_idx
    then Vec.Value.push t.clause_sorting_buckets clause_idx);
  let compare_by_score a b =
    F64.compare
      (Clause.activity (Clause.Pool.get t.clauses (Ptr.of_int a)))
      (Clause.activity (Clause.Pool.get t.clauses (Ptr.of_int b)))
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
    free_clause t (Ptr.of_int clause_idx)
  done
;;

let rec try_unit_propagate t = exclave_
  match Vec.Value.last t.pending_units with
  | None -> None
  | Some (clause_idx, generation, literal) ->
    ignore (Vec.Value.pop_exn t.pending_units);
    let clause = Clause.Pool.get t.clauses (Ptr.of_int clause_idx) in
    if Clause.pending_unit_generation clause = generation
    then Clause.set_pending_unit_generation clause (-1);
    if Clause.deleted clause || Clause.generation clause <> generation
    then try_unit_propagate t
    else (
      let literal = Literal.of_int literal in
      match assignment t ~var:(Literal.var literal) with
      | Some value ->
        if Bool.equal value (Literal.value literal)
        then try_unit_propagate t
        else Some clause_idx
      | None ->
        if t.debug
        then
          print_s
            [%message
              "try_unit_propagate: found unit"
                ~clause:(Clause.to_int_array clause : int array)
                ~assignments:(assignments_array t : int array)
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
         | Null -> try_unit_propagate t
         | This failed_clause_idx -> Some failed_clause_idx))
;;

let restart t = exclave_
  t.conflicts <- #0L;
  t.decision_level <- #0L;
  clear_pending_units t;
  while
    Trail_entry.Vec.length t.trail <> 0
    && I64.O.(
         (Trail_entry.Vec.last_exn t.trail).#decision_level
         > t.decision_level_of_last_assumption)
  do
    undo_entry t ~trail_entry:(Trail_entry.Vec.pop_exn t.trail)
  done;
  Clause.Pool.iter t.clauses ~f:(fun ptr ->
    let clause = Clause.Pool.get t.clauses ptr in
    match%optional_u
      (Clause.unit_literal clause ~assignments:t.assignments : Literal.Option.t)
    with
    | None -> ()
    | Some literal -> queue_pending_unit t ~clause_idx:(Ptr.to_int ptr) ~literal)
;;

let%template unsat t failed_clause_idx : Sat_result.t @ m =
  (let failed_clause =
     Clause.Pool.get t.clauses (Ptr.of_int failed_clause_idx)
   in
   let learned_clause =
     (* raise when it's conflicts from a unit clause we deduced. *)
     try
       let clause = learn_clause_from_failure t ~failed_clause in
       Clause.negate clause;
       clause
     with
     | _ -> Clause.copy failed_clause
   in
   Unsat { unsat_core = learned_clause })
  [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
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
   let learn_from_failure failed_clause_idx : Sat_result.t @ m =
     match[@exclave_if_stack a]
       I64.O.(
         t.decision_level = #0L
         || t.decision_level < t.decision_level_of_last_assumption)
     with
     | true -> (unsat [@alloc a]) t failed_clause_idx
     | false ->
       let failed_clause =
         Clause.Pool.get t.clauses (Ptr.of_int failed_clause_idx)
       in
       backtrack t ~failed_clause;
       t.conflicts <- I64.O.(t.conflicts + #1L);
       if I64.O.(
            t.conflicts >= Luby.value t.luby
            && I64.O.(t.decision_level <> t.decision_level_of_last_assumption))
       then (
         ignore (Luby.next t.luby);
         restart t);
       (solve' [@alloc a]) t
   in
   match try_unit_propagate t with
   | Some failed_clause_idx -> learn_from_failure failed_clause_idx
   | None ->
     (match (make_decision [@alloc a]) t with
      | `Continue -> (solve' [@alloc a]) t
      | `Failed_clause failed_clause_idx -> learn_from_failure failed_clause_idx
      | `Done sat_result -> (check_sat_result [@alloc a]) t ~sat_result))
  [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let add_assumptions ~(local_ assumptions) t = exclave_
  let rec go i = exclave_
    if i = Array.length assumptions
    then `Continue
    else (
      t.iterations <- t.iterations + 1;
      let literal = Literal.of_int assumptions.(i) in
      match try_unit_propagate t with
      | Some failed_clause_idx -> `Failed_clause failed_clause_idx
      | None ->
        (match assignment t ~var:(Literal.var literal) with
         | None ->
           (match
              (make_decision' [@alloc stack]) ~is_assumption:true t ~literal
            with
            | `Failed_clause _ as res -> res
            | `Continue -> go (i + 1))
         | Some b ->
           if Bool.equal b (Literal.value literal)
           then go (i + 1)
           else (
             let trail_entry_idx =
               I64.Option.Vec.get t.trail_entry_idx_by_var (Literal.var literal)
               |> I64.Option.value ~default:#0L (* always [some] *)
             in
             let trail_entry =
               Trail_entry.Vec.get t.trail (I64.to_int_trunc trail_entry_idx)
             in
             match trail_entry.#reason with
             | T #(Decision, _, _) -> failwith "invalid assumptions"
             | T #(Clause_idx, failed_clause_idx, _) ->
               `Failed_clause failed_clause_idx)))
  in
  go 0
;;

let%template solve ?(local_ assumptions = [||]) t : Sat_result.t @ m =
  t.decision_level_of_last_assumption <- #0L;
  if t.debug
  then
    print_s
      [%message
        "solve" ~assumptions:([%globalize: int array] assumptions : int array)];
  if t.iterations > 0
  then (
    t.iterations <- 0;
    restart t);
  if t.has_empty_clause
  then Unsat { unsat_core = Clause.of_int_array [||] }
  else (
    match[@exclave_if_stack a] add_assumptions ~assumptions t with
    | `Continue -> (solve' [@alloc a]) t
    | `Failed_clause failed_clause_idx -> (unsat [@alloc a]) t failed_clause_idx)
[@@alloc a @ m = (stack_local, heap_global)]
;;

let create ?(debug = false) () =
  { has_empty_clause = false
  ; decision_level = #0L
  ; decision_level_of_last_assumption = #0L
  ; iterations = 0
  ; assignments = Tf_pair.create (fun (_ : bool) -> Bitset.create ())
  ; trail = Trail_entry.Vec.create ()
  ; clauses = Clause.Pool.create ~chunk_size:4096 ()
  ; pending_units = Vec.Value.create ()
  ; clauses_with_active_unit = Int.H_set.create ()
  ; clause_adjusting_score = Adjusting_score.default ()
  ; clauses_by_literal = Tf_pair.create (fun (_ : bool) -> Vec.Value.create ())
  ; trail_entry_idx_by_var = I64.Option.Vec.create ()
  ; watched_clauses_by_literal =
      Tf_pair.create (fun (_ : bool) -> Vec.Value.create ())
  ; vsids = Vsids.create ()
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

let create_with_formula ?debug formula =
  let t = create ?debug () in
  Array.fold formula ~init:t ~f:(fun t clause -> add_clause' t ~clause)
;;
