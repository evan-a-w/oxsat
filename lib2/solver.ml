open! Core
open! Import
open! Stdlib_stable

type t =
  { trail : Trail_entry.Vec.t
  ; mutable trail_processed_till : int
  ; mutable decision_level : int
  ; mutable decision_level_of_last_assumption : int
  ; clauses : Clause.t Vec.Value.t
  ; vars : Var.t Vec.Value.t
  ; unassigned_literals : Literal_set.t
  ; mutable has_empty_clause : bool
  ; mutable stats : Stats.t
  ; analyze_conflict_stamp_set : Stamp_set.t
  ; analyze_conflict_scratch_literals : int Vec.Value.t
  ; debug : bool
  }

let stats t = t.stats
let literal_var t ~literal = Vec.Value.get t.vars (Int.abs literal)
let clause_to_array clause = Vec.Value.to_array clause.Clause.clause

let clause_with_assignments t ~clause =
  clause_to_array clause
  |> Array.map ~f:(fun literal ->
    let var = literal_var t ~literal in
    literal, var.assignment)
;;

let assignments_array t : bool option array =
  Vec.Value.map t.vars ~f:(fun (var : Var.t) ->
    if var.exists then Or_null.to_option var.assignment else None)
  |> Vec.Value.to_array
;;

let undo_entry t ~(trail_entry : Trail_entry.t) =
  if t.debug
  then
    print_s
      [%message
        "undo_entry" (trail_entry.#literal : int) (t.decision_level : int)];
  let literal = trail_entry.#literal in
  let var = literal_var t ~literal in
  Literal_set.insert t.unassigned_literals ~literal:trail_entry.#literal;
  Literal_set.insert t.unassigned_literals ~literal:(-trail_entry.#literal);
  var.assignment <- Null;
  var.trail_entry <- Trail_entry.Option_u.none ();
  match trail_entry.#reason with
  | T #(Decision, ()) -> ()
  | T #(Clause_idx, clause_idx) ->
    (Vec.Value.get t.clauses clause_idx).has_unit <- false
;;

let pop_from_trail_exn t =
  let trail_entry = Trail_entry.Vec.pop_exn t.trail in
  undo_entry t ~trail_entry
;;

let push_trail_entry t ~(trail_entry : Trail_entry.t) =
  let literal = trail_entry.#literal in
  let var = literal_var t ~literal in
  match var.assignment with
  | This b when Bool.equal b (literal > 0) -> `Duplicate_in_trail
  | This _ -> `Conflict_in_trail
  | Null ->
    if t.debug
    then
      print_s
        [%message
          "add_to_trail"
            (trail_entry.#literal : int)
            (trail_entry.#decision_level : int)
            (t.decision_level : int)];
    var.assignment <- This (trail_entry.#literal > 0);
    var.trail_entry <- Trail_entry.Option_u.some trail_entry;
    Trail_entry.Vec.push t.trail trail_entry;
    Literal_set.remove t.unassigned_literals ~literal:trail_entry.#literal;
    Literal_set.remove t.unassigned_literals ~literal:(-trail_entry.#literal);
    (match trail_entry.#reason with
     | T #(Decision, ()) -> ()
     | T #(Clause_idx, clause_idx) ->
       (Vec.Value.get t.clauses clause_idx).has_unit <- true);
    `Added_to_trail
;;

let push_unit_trail_entry t ~literal ~clause_idx =
  if t.debug
  then (
    let clause = Vec.Value.get t.clauses clause_idx in
    print_s
      [%message
        "found unit"
          ~clause:
            (clause_with_assignments t ~clause : (int * bool or_null) array)
          (literal : int)]);
  match
    push_trail_entry
      t
      ~trail_entry:
        (#{ decision_level = t.decision_level
          ; literal
          ; reason = Reason.clause_idx clause_idx
          }
         : Trail_entry.t)
  with
  | `Added_to_trail | `Duplicate_in_trail -> Null
  | `Conflict_in_trail -> This clause_idx
;;

let is_satisfied t ~literal =
  let var = literal_var t ~literal in
  [%equal: bool or_null] var.assignment (This (literal > 0))
;;

let replace_watched_literal' t ~clause_idx ~nullified_literal = exclave_
  let clause = Vec.Value.get t.clauses clause_idx in
  assert (
    nullified_literal = Vec.Value.get clause.clause 0
    || nullified_literal = Vec.Value.get clause.clause 1);
  if Vec.Value.length clause.clause = 1
  then `Not_replaced_conflict clause_idx
  else (
    if Vec.Value.get clause.clause 0 = nullified_literal
    then Vec.Value.swap clause.clause 0 1;
    assert (Vec.Value.get clause.clause 1 = nullified_literal);
    let other_literal = Vec.Value.get clause.clause 0 in
    let other_var = literal_var t ~literal:other_literal in
    if is_satisfied t ~literal:other_literal
    then
      (* already satisfied, do nothing *)
      `Not_replaced_not_conflict
        (* else if not (Or_null.is_null other_var.assignment) *)
        (* then *)
        (*   (\* other watched literal is already assigned, so there can't be a *)
        (*      replacement, so this is a conflict *\) *)
        (*   `Not_replaced_conflict clause_idx *)
    else (
      let rec go i = exclave_
        if i >= Vec.Value.length clause.clause
        then `No_replacement_found
        else (
          let literal = Vec.Value.get clause.clause i in
          let var = literal_var t ~literal in
          if is_satisfied t ~literal
          then `Already_satisfied
          else (
            match var.assignment with
            | This _ -> go (i + 1)
            | Null ->
              (* found a replacement *)
              (* TODO: this might already be in the trail, ideally we take ones that aren't in the trail *)
              `Replacement (~var:{ global = var }, ~literal, ~i)))
      in
      match go 2 with
      | `Already_satisfied -> `Not_replaced_not_conflict
      | `No_replacement_found ->
        (match other_var.assignment with
         | Null ->
           (* other watched literal is a unit *)
           (match
              push_unit_trail_entry t ~literal:other_literal ~clause_idx
            with
            | Null -> `Not_replaced_not_conflict
            | This clause_idx -> `Not_replaced_conflict clause_idx)
         | This _ -> `Not_replaced_conflict clause_idx)
      | `Replacement (~var:{ global = var }, ~literal, ~i) ->
        assert (literal <> nullified_literal);
        Vec.Value.swap clause.clause 1 i;
        (* guaranteed non binary *)
        Watched_clause.Vec.push
          (Tf_pair.get var.watched_clauses (literal > 0))
          (Watched_clause.create
             ~clause_idx
             ~blocking_literal:other_literal
             ~is_binary:false);
        `Replaced literal))
;;

let replace_watched_literal t ~clause_idx ~nullified_literal = exclave_
  let res = replace_watched_literal' t ~clause_idx ~nullified_literal in
  if t.debug
  then (
    let clause = Vec.Value.get t.clauses clause_idx in
    print_s
      [%message
        "replaced_watched_literal"
          (nullified_literal : int)
          (clause_with_assignments t ~clause : (int * bool or_null) array)
          (res
           : [ `Not_replaced_not_conflict
             | `Not_replaced_conflict of int
             | `Replaced of int
             ])]);
  res
;;

let update_watches_for_assignment t ~(var : Var.t) ~literal = exclave_
  if t.debug
  then print_s [%message "update_watches_for_assignment" (literal : int)];
  (* other assignment invalidated *)
  let watched_clauses = Tf_pair.get var.watched_clauses (not (literal > 0)) in
  let found_conflict = stack_ (ref false) in
  let conflict_clause = stack_ (ref (-1)) in
  Watched_clause.Vec.filter_inplace
    watched_clauses
    ~f:(fun #{ clause_idx; blocking_literal; is_binary } ->
      if !found_conflict
      then (* just do nothing if already conflict *)
        true
      else if is_satisfied t ~literal:blocking_literal
      then true (* already satisfied, do nothing *)
      else if is_binary
      then (
        let blocking_var = literal_var t ~literal:blocking_literal in
        match blocking_var.assignment with
        | Null ->
          (* immediately add unit literal, because binary and this literal isn't
             satisfied *)
          (match
             push_unit_trail_entry t ~literal:blocking_literal ~clause_idx
           with
           | Null -> true
           | This clause_idx ->
             found_conflict := true;
             conflict_clause := clause_idx;
             true)
        | This _ ->
          found_conflict := true;
          conflict_clause := clause_idx;
          true)
      else (
        match
          replace_watched_literal t ~clause_idx ~nullified_literal:(-literal)
        with
        | `Replaced _ -> false
        | `Not_replaced_not_conflict | `Clause_too_short -> true
        | `Not_replaced_conflict clause_idx ->
          found_conflict := true;
          conflict_clause := clause_idx;
          true));
  match !found_conflict with
  | true -> `Conflict !conflict_clause
  | false -> `No_conflict
;;

let rec remove_greater_than_decision_level t ~decision_level =
  let rec undo_while_unprocessed () =
    if t.trail_processed_till >= Trail_entry.Vec.length t.trail
    then ()
    else (
      pop_from_trail_exn t;
      undo_while_unprocessed ())
  in
  undo_while_unprocessed ();
  let remaining_level =
    if Trail_entry.Vec.length t.trail = 0
    then 0
    else (Trail_entry.Vec.last_exn t.trail).#decision_level
  in
  if remaining_level <= decision_level
  then (
    t.decision_level <- remaining_level;
    t.trail_processed_till <- Trail_entry.Vec.length t.trail)
  else (
    pop_from_trail_exn t;
    remove_greater_than_decision_level t ~decision_level)
;;

let rec propagate t : int or_null =
  match t.trail_processed_till < Trail_entry.Vec.length t.trail with
  | false -> Null
  | true ->
    let trail_entry = Trail_entry.Vec.get t.trail t.trail_processed_till in
    t.trail_processed_till <- t.trail_processed_till + 1;
    let var = Vec.Value.get t.vars (Int.abs trail_entry.#literal) in
    if t.debug
    then
      print_s
        [%message "propagate: trying assignment" (trail_entry.#literal : int)];
    (match
       update_watches_for_assignment t ~var ~literal:trail_entry.#literal
     with
     | `Conflict clause_idx -> This clause_idx
     | `No_conflict ->
       t.stats <- #{ t.stats with propagations = t.stats.#propagations + 1 };
       propagate t)
;;

let register_watcher t ~literal ~clause_idx ~blocking_literal =
  let var = literal_var t ~literal in
  Watched_clause.Vec.push
    (Tf_pair.get var.watched_clauses (literal > 0))
    (Watched_clause.create ~clause_idx ~blocking_literal ~is_binary:false)
;;

let register_watchers_for_new_clause t ~literals ~clause_idx =
  let lit1 = Vec.Value.get literals 0 in
  let lit2 = Vec.Value.get literals 1 in
  register_watcher t ~literal:lit1 ~clause_idx ~blocking_literal:lit2;
  register_watcher t ~literal:lit2 ~clause_idx ~blocking_literal:lit1
;;

let ensure_literal t ~literal =
  while Vec.Value.length t.vars <= Int.abs literal do
    Vec.Value.push
      t.vars
      { assignment = Null
      ; trail_entry = Trail_entry.Option_u.none ()
      ; watched_clauses = Tf_pair.create (fun _ -> Watched_clause.Vec.create ())
      ; exists = false
      }
  done;
  let var = literal_var t ~literal in
  if not var.exists
  then (
    Literal_set.insert t.unassigned_literals ~literal;
    Literal_set.insert t.unassigned_literals ~literal:(-literal);
    var.exists <- true)
;;

let add_clause t ~literals ~learned =
  let len = Vec.Value.length literals in
  if len = 0 then t.has_empty_clause <- true;
  let satisfied = stack_ (ref false) in
  let num_unassigned = stack_ (ref 0) in
  let satisfied_at_front = stack_ (ref 0) in
  (* figure out if satisfied, and place unassigned variables at the front if not satisfied, and satisfied variables at the front if satisfied *)
  let rec go i =
    if i >= Vec.Value.length literals
    then ()
    else (
      let literal = Vec.Value.get literals i in
      ensure_literal t ~literal;
      let var = literal_var t ~literal in
      let satisfied_by_this = is_satisfied t ~literal in
      if satisfied_by_this && !satisfied_at_front <= 1
      then (
        Vec.Value.swap literals !satisfied_at_front i;
        incr satisfied_at_front);
      satisfied := !satisfied || satisfied_by_this;
      (match var.assignment with
       | This _ -> ()
       | Null ->
         if !num_unassigned <= 1 && not satisfied_by_this
         then Vec.Value.swap literals !num_unassigned i;
         incr num_unassigned);
      go (i + 1))
  in
  go 0;
  (* has unit is populated when the trail entry is seen *)
  let clause : Clause.t = { clause = literals; has_unit = false; learned } in
  let clause_idx = Vec.Value.length t.clauses in
  Vec.Value.push t.clauses clause;
  if len >= 2 then register_watchers_for_new_clause t ~literals ~clause_idx;
  match
    (!num_unassigned = 1 || Vec.Value.length literals = 1) && not !satisfied
  with
  | false -> `Ok
  | true ->
    (* add unit *)
    (match
       push_unit_trail_entry t ~literal:(Vec.Value.get literals 0) ~clause_idx
     with
     | Null -> `Ok
     | This _ -> `Conflict clause_idx)
;;

let mark_literal t ~seen ~literal ~(local_ path_count) ~learned_literals =
  let var = Int.abs literal in
  if not (Stamp_set.is_seen seen ~var)
  then (
    Stamp_set.mark_seen seen ~var;
    let var = literal_var t ~literal in
    match var.assignment with
    | Null -> ()
    | This _ ->
      (match%optional_u (var.trail_entry : Trail_entry.Option_u.t) with
       | None -> ()
       | Some trail_entry ->
         let dl = trail_entry.#decision_level in
         (* if dl = 0 then () else *)
         if dl = t.decision_level
         then incr path_count
         else Vec.Value.push learned_literals literal))
;;

let simplify_learned_clause t ~learned_literals ~uip_literal ~seen =
  (* just don't add redundant literals

     redundant if the learned clause is implied by the literals we've already
     seen *)
  let backjump_level = ref 0 in
  let new_learned_literals =
    Vec.Value.of_array_taking_ownership [| uip_literal |]
  in
  let rec go i =
    if i >= Vec.Value.length learned_literals
    then ()
    else (
      let literal = Vec.Value.get learned_literals i in
      let var = literal_var t ~literal in
      match%optional_u (var.trail_entry : Trail_entry.Option_u.t) with
      | None -> go (i + 1)
      | Some trail_entry ->
        let skip =
          match trail_entry.#reason with
          | T #(Decision, ()) -> false
          | T #(Clause_idx, clause_idx) ->
            let reason = Vec.Value.get t.clauses clause_idx in
            let all_marked = ref (Vec.Value.length reason.clause > 1) in
            Vec.Value.iter reason.clause ~f:(fun literal ->
              if Int.abs literal = Int.abs uip_literal
              then ()
              else if not (Stamp_set.is_seen seen ~var:(Int.abs literal))
              then all_marked := false);
            !all_marked
        in
        if skip
        then ()
        else (
          let dl = trail_entry.#decision_level in
          Vec.Value.push new_learned_literals literal;
          if !backjump_level < dl
          then (
            backjump_level := dl;
            Vec.Value.swap
              new_learned_literals
              1
              (Vec.Value.length new_learned_literals - 1)));
        go (i + 1))
  in
  go 1;
  #(~learned_literals:new_learned_literals, ~backjump_level:!backjump_level)
;;

let analyze_conflict t ~(failed_clause : Clause.t) =
  let seen = t.analyze_conflict_stamp_set in
  Stamp_set.reset seen;
  let learned_literals = t.analyze_conflict_scratch_literals in
  Vec.Value.clear learned_literals;
  let path_count = stack_ (ref 0) in
  let mark_literal literal =
    mark_literal t ~seen ~literal ~learned_literals ~path_count
  in
  Vec.Value.iter failed_clause.clause ~f:mark_literal;
  let failed_clause_with_assignments =
    clause_with_assignments t ~clause:failed_clause
  in
  if t.debug
  then
    print_s
      [%message
        "analyze_conflict"
          ~failed_clause:(clause_to_array failed_clause : int array)
          (failed_clause_with_assignments : (int * bool or_null) array)];
  let found_uip = ref false in
  let uip_literal = ref 0 in
  let i = ref (Trail_entry.Vec.length t.trail - 1) in
  while !i >= 0 && not !found_uip do
    let trail_entry = Trail_entry.Vec.get t.trail !i in
    let literal = trail_entry.#literal in
    if Stamp_set.is_seen seen ~var:(Int.abs literal)
       && trail_entry.#decision_level = t.decision_level
    then (
      Stamp_set.clear_seen seen ~var:(Int.abs literal);
      decr path_count;
      if !path_count = 0
      then (
        found_uip := true;
        uip_literal := -literal)
      else (
        match trail_entry.#reason with
        | T #(Decision, ()) -> failwith "found decision before reaching UIP"
        | T #(Clause_idx, clause_idx) ->
          let clause = Vec.Value.get t.clauses clause_idx in
          if t.debug
          then
            print_s
              [%message
                "analyze_conflict"
                  ~learned:(Vec.Value.to_array learned_literals : int array)
                  ~see:(clause_to_array clause : int array)
                  (literal : int)
                  (!path_count : int)];
          Vec.Value.iter clause.clause ~f:(fun reason_literal ->
            if Int.abs reason_literal <> Int.abs literal
            then mark_literal reason_literal)));
    decr i
  done;
  if not !found_uip then failwith "conflict analysis failed to find UIP";
  let uip_literal = !uip_literal in
  if Vec.Value.length learned_literals = 0
  then Vec.Value.push learned_literals uip_literal
  else (
    Vec.Value.push learned_literals (Vec.Value.get learned_literals 0);
    Vec.Value.set learned_literals 0 uip_literal);
  simplify_learned_clause ~learned_literals ~uip_literal ~seen t
;;

let rec backtrack t ~failed_clause =
  let #(~learned_literals, ~backjump_level) =
    analyze_conflict t ~failed_clause
  in
  t.stats
  <- #{ t.stats with
        learned_clauses = t.stats.#learned_clauses + 1
      ; learned_clause_literals =
          Vec.Value.length learned_literals + t.stats.#learned_clause_literals
      };
  if t.debug
  then
    print_s
      [%message
        "backtrack"
          ~learned_clause:(Vec.Value.to_list learned_literals : int list)];
  remove_greater_than_decision_level t ~decision_level:backjump_level;
  match add_clause t ~literals:learned_literals ~learned:true with
  | `Ok -> ()
  | `Conflict failed_clause_idx ->
    backtrack t ~failed_clause:(Vec.Value.get t.clauses failed_clause_idx)
;;

let unsat_core t failed_clause_idx =
  let failed_clause = Vec.Value.get t.clauses failed_clause_idx in
  let #(~learned_literals, ~backjump_level:_) =
    analyze_conflict t ~failed_clause
  in
  Vec.Value.map_inplace learned_literals ~f:(fun x -> -x);
  Vec.Value.to_array learned_literals
;;

let%template unsat t failed_clause_idx : Sat_result.t @ m =
  let unsat_core = unsat_core t failed_clause_idx in
  Unsat { unsat_core } [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let make_decision' ~is_assumption t ~literal =
  if t.debug then print_s [%message "make_decision" (literal : int)];
  if not is_assumption
  then t.stats <- #{ t.stats with decisions = t.stats.#decisions + 1 };
  t.decision_level <- t.decision_level + 1;
  if is_assumption then t.decision_level_of_last_assumption <- t.decision_level;
  let trail_entry : Trail_entry.t =
    #{ reason = Reason.decision (); literal; decision_level = t.decision_level }
  in
  let var = literal_var t ~literal in
  assert (Or_null.is_null var.assignment);
  match push_trail_entry t ~trail_entry with
  | `Conflict_in_trail | `Duplicate_in_trail ->
    Error.raise_s [%message "[make_decision'] conflicting with trail"]
  | `Added_to_trail -> propagate t
;;

let%template make_decision t : _ @ m =
  match Literal_set.pop_one t.unassigned_literals with
  | Null -> `Done (Sat_result.Sat { assignments = assignments_array t })
  | This literal ->
    (match[@exclave_if_stack a]
       make_decision' ~is_assumption:false t ~literal
     with
     | Null -> `Continue
     | This clause_idx -> `Failed_clause clause_idx)
[@@alloc a @ m = (stack_local, heap_global)]
;;

let%template check_sat_result t ~(sat_result : _ @ m) : _ @ m =
  match (sat_result : Sat_result.t) with
  | Unsat _ -> sat_result
  | Sat { assignments } ->
    if t.debug || true
    then
      Vec.Value.iter t.clauses ~f:(fun clause ->
        let is_satisfied = ref false in
        Vec.Value.iter clause.clause ~f:(fun literal ->
          is_satisfied
          := !is_satisfied
             || [%equal: bool option]
                  assignments.(Int.abs literal)
                  (Some (literal > 0)));
        if not !is_satisfied
        then
          Error.raise_s
            [%message
              "BUG: clause isn't satisfied in result"
                (clause.clause : int Vec.Value.t)]);
    sat_result
[@@alloc a @ m = (stack_local, heap_global)]
;;

let%template rec solve' t : Sat_result.t @ m =
  (t.stats <- #{ t.stats with iterations = t.stats.#iterations + 1 };
   match propagate t with
   | This failed_clause_idx -> learn_from_failure t ~failed_clause_idx
   | Null ->
     (match (make_decision [@alloc a]) t with
      | `Continue -> (solve' [@alloc a]) t
      | `Failed_clause failed_clause_idx ->
        (learn_from_failure [@alloc a]) t ~failed_clause_idx
      | `Done sat_result -> (check_sat_result [@alloc a]) t ~sat_result))
  [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]

and learn_from_failure t ~failed_clause_idx : Sat_result.t @ m =
  match[@exclave_if_stack a]
    t.decision_level = 0
    || t.decision_level < t.decision_level_of_last_assumption
  with
  | true -> (unsat [@alloc a]) t failed_clause_idx
  | false ->
    let failed_clause = Vec.Value.get t.clauses failed_clause_idx in
    backtrack t ~failed_clause;
    t.stats <- #{ t.stats with conflicts = t.stats.#conflicts + 1 };
    (solve' [@alloc a]) t
[@@alloc a @ m = (stack_local, heap_global)]
;;

let restart t = exclave_
  t.stats <- #{ t.stats with conflicts = 0 };
  t.decision_level <- t.decision_level_of_last_assumption;
  while
    Trail_entry.Vec.length t.trail <> 0
    && (Trail_entry.Vec.last_exn t.trail).#decision_level
       > t.decision_level_of_last_assumption
  do
    pop_from_trail_exn t
  done;
  t.trail_processed_till <- Trail_entry.Vec.length t.trail
;;

let add_assumptions ~(local_ assumptions) t = exclave_
  let rec go i = exclave_
    if i = Array.length assumptions
    then Null
    else (
      t.stats <- #{ t.stats with iterations = t.stats.#iterations + 1 };
      let literal = assumptions.(i) in
      let var = literal_var t ~literal in
      match var.assignment with
      | Null ->
        (match make_decision' ~is_assumption:true t ~literal with
         | This _ as res -> res
         | Null -> go (i + 1))
      | This b ->
        if Bool.equal b (literal > 0)
        then go (i + 1)
        else (
          let trail_entry = Trail_entry.Option_u.value_exn var.trail_entry in
          match trail_entry.#reason with
          | T #(Decision, ()) -> failwith "invalid assumptions"
          | T #(Clause_idx, failed_clause_idx) -> This failed_clause_idx))
  in
  match go 0 with
  | Null -> `Continue
  | This i -> `Failed_clause i
;;

let%template solve ?(local_ assumptions = [||]) t : Sat_result.t @ m =
  t.decision_level_of_last_assumption <- 0;
  if t.debug
  then
    print_s
      [%message
        "solve" ~assumptions:([%globalize: int array] assumptions : int array)];
  let has_run_before = t.stats.#iterations > 0 in
  t.stats <- Stats.empty ();
  if has_run_before
  then (
    if t.debug then print_endline "restarting";
    restart t);
  if t.has_empty_clause
  then Unsat { unsat_core = [||] }
  else (
    match[@exclave_if_stack a] add_assumptions ~assumptions t with
    | `Continue -> (solve' [@alloc a]) t
    | `Failed_clause failed_clause_idx -> (unsat [@alloc a]) t failed_clause_idx)
[@@alloc a @ m = (stack_local, heap_global)]
;;

let create ?(random_state = Random.State.make [| 1; 2; 3 |]) ?(debug = false) ()
  =
  { trail = Trail_entry.Vec.create ()
  ; trail_processed_till = 0
  ; decision_level = 0
  ; decision_level_of_last_assumption = 0
  ; clauses = Vec.Value.create ()
  ; vars = Vec.Value.create ()
  ; unassigned_literals = Literal_set.create ~random_state
  ; has_empty_clause = false
  ; stats = Stats.empty ()
  ; analyze_conflict_stamp_set = Stamp_set.create ()
  ; analyze_conflict_scratch_literals = Vec.Value.create ()
  ; debug
  }
;;

let add_clause t ~clause =
  match
    add_clause
      t
      ~literals:(Vec.Value.of_array_taking_ownership clause)
      ~learned:false
  with
  | `Ok -> `Ok
  | `Conflict failed_clause_idx -> `Unsat (unsat_core t failed_clause_idx)
;;

let create_with_formula ?(local_ debug) formula =
  let t = create ?debug () in
  let unsat_core = stack_ (ref Null) in
  Array.iter formula ~f:(fun clause ->
    match !unsat_core with
    | This _ -> ()
    | Null ->
      (match add_clause t ~clause with
       | `Ok -> ()
       | `Unsat core -> unsat_core := This core));
  match !unsat_core with
  | Null -> `Ok t
  | This core -> `Unsat core
;;
