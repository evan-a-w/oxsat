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
  ; mutable has_empty_clause : bool
  ; mutable stats : Stats.t
  ; analyze_conflict_stamp_set : Stamp_set.t
  ; analyze_conflict_scratch_literals : int Vec.Value.t
  ; debug : bool
  ; vsids : Vsids.t
  ; mutable luby_index : int
  ; mutable conflicts_since_restart : int
  ; lbd_stamp_set : Stamp_set.t
  ; mutable clause_act_inc : float
  ; mutable iterations : int
  ; theory : Theory.Packed.t or_null
  ; mutable theory_reported_consistent : bool
  }

type time_bound =
  [ `Unlimited
  | `Bounded of int
  ]

exception Timeout

let timeout_check_every = 1024

module Timer = struct
  type t =
    { deadline : Time_ns.t option
    ; mutable iterations_until_check : int
    }

  let create = function
    | `Unlimited ->
      { deadline = None; iterations_until_check = Core.Int.max_value }
    | `Bounded ms ->
      let deadline =
        Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_ms (Float.of_int ms))
      in
      { deadline = Some deadline; iterations_until_check = 0 }
  ;;

  let check t =
    match t.deadline with
    | None -> ()
    | Some deadline ->
      if t.iterations_until_check > 0
      then t.iterations_until_check <- t.iterations_until_check - 1
      else (
        t.iterations_until_check <- timeout_check_every;
        if Time_ns.(now () >= deadline) then raise Timeout)
  ;;
end

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

let assignments_array_pretty t =
  let res =
    Vec.Value.filter_mapi t.vars ~f:(fun i (var : Var.t) ->
      if not var.exists
      then None
      else (
        match var.assignment with
        | Null -> None
        | This true -> Some i
        | This false -> Some (-i)))
    |> Vec.Value.to_array
  in
  Array.sort res ~compare:Int.compare;
  res
;;

let _ = assignments_array_pretty

let undo_entry t ~(trail_entry : Trail_entry.t) =
  if t.debug
  then
    print_s
      [%message
        "undo_entry" (trail_entry.#literal : int) (t.decision_level : int)];
  let literal = trail_entry.#literal in
  let var = literal_var t ~literal in
  Vsids.add_to_pool t.vsids ~literal:trail_entry.#literal;
  var.assignment <- Null;
  var.trail_entry <- Trail_entry.Option_u.none ();
  match trail_entry.#reason with
  | T #(Decision, ()) -> ()
  | T #(Clause_idx, clause_idx) ->
    (Vec.Value.get t.clauses clause_idx).has_unit <- false
;;

let pop_from_trail_exn t =
  let trail_entry = Trail_entry.Vec.pop_exn t.trail in
  let to_decision_level =
    match Trail_entry.Vec.length t.trail with
    | 0 -> 0
    | l -> (Trail_entry.Vec.get t.trail (l - 1)).#decision_level
  in
  if to_decision_level <> trail_entry.#decision_level
  then (
    match t.theory with
    | Null -> ()
    | This (T ((module Theory), theory)) ->
      Theory.undo theory ~to_decision_level_excl:to_decision_level;
      t.theory_reported_consistent <- false);
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
    (match t.theory with
     | Null -> ()
     | This (T ((module Theory), theory)) ->
       t.theory_reported_consistent <- false;
       Theory.assert_literal
         theory
         ~decision_level:trail_entry.#decision_level
         ~literal);
    var.assignment <- This (trail_entry.#literal > 0);
    var.trail_entry <- Trail_entry.Option_u.some trail_entry;
    Trail_entry.Vec.push t.trail trail_entry;
    Vsids.remove_from_pool t.vsids ~var:(Int.abs trail_entry.#literal);
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

let update_blocker_for_watcher t ~watched_literal ~clause_idx ~blocking_literal =
  let var = literal_var t ~literal:watched_literal in
  let watched_clauses = Tf_pair.get var.watched_clauses (watched_literal > 0) in
  Watched_clause.Vec.iteri watched_clauses ~f:(fun i watched_clause ->
    if watched_clause.#clause_idx = clause_idx
    then
      Watched_clause.Vec.set
        watched_clauses
        i
        (Watched_clause.create
           ~clause_idx
           ~blocking_literal
           ~is_binary:watched_clause.#is_binary))
;;

let replace_watched_literal t ~clause_idx ~nullified_literal = exclave_
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
        (* (\* other watched literal is already assigned, so there can't be a *)
        (* replacement, so this is a conflict *\) *)
        (* `Not_replaced_conflict clause_idx *)
    else (
      let rec go i = exclave_
        if i >= Vec.Value.length clause.clause
        then `No_replacement_found
        else (
          let literal = Vec.Value.get clause.clause i in
          let var = literal_var t ~literal in
          if is_satisfied t ~literal
          then `Replacement (~var:{ global = var }, ~literal, ~i)
          else (
            match var.assignment with
            | This _ -> go (i + 1)
            | Null ->
              (* found a replacement *)
              (* TODO: this might already be in the trail, ideally we take ones
                 that aren't in the trail *)
              `Replacement (~var:{ global = var }, ~literal, ~i)))
      in
      match go 2 with
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
        update_blocker_for_watcher
          t
          ~watched_literal:other_literal
          ~clause_idx
          ~blocking_literal:literal;
        (* guaranteed non binary *)
        Watched_clause.Vec.push
          (Tf_pair.get var.watched_clauses (literal > 0))
          (Watched_clause.create
             ~clause_idx
             ~blocking_literal:other_literal
             ~is_binary:false);
        `Replaced literal))
;;

let update_watches_for_assignment t ~(var : Var.t) ~literal = exclave_
  if t.debug
  then print_s [%message "update_watches_for_assignment" (literal : int)];
  (* other assignment invalidated *)
  let watched_clauses = Tf_pair.get var.watched_clauses (not (literal > 0)) in
  let conflict_clause = stack_ (ref Null) in
  Watched_clause.Vec.filter_inplace
    watched_clauses
    ~f:(fun #{ clause_idx; blocking_literal; is_binary } ->
      if (Vec.Value.get t.clauses clause_idx).deleted
      then false (* eagerly remove deleted clauses from watch lists *)
      else if not (Or_null.is_null !conflict_clause)
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
             conflict_clause := This clause_idx;
             true)
        | This _ ->
          conflict_clause := This clause_idx;
          true)
      else (
        match
          replace_watched_literal t ~clause_idx ~nullified_literal:(-literal)
        with
        | `Replaced _ -> false
        | `Not_replaced_not_conflict | `Clause_too_short -> true
        | `Not_replaced_conflict clause_idx ->
          conflict_clause := This clause_idx;
          true));
  match !conflict_clause with
  | This conflict_clause -> `Conflict conflict_clause
  | Null -> `No_conflict
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

let trail_index_of_var_exn t ~var =
  let rec go i =
    if i < 0
    then
      Error.raise_s
        [%message "BUG: assigned var missing from trail" (var : int)]
    else if Int.abs (Trail_entry.Vec.get t.trail i).#literal = var
    then i
    else go (i - 1)
  in
  go (Trail_entry.Vec.length t.trail - 1)
;;

let conflict_keep_trail_len t ~failed_clause_idx =
  let failed_clause = Vec.Value.get t.clauses failed_clause_idx in
  let keep = ref t.trail_processed_till in
  Vec.Value.iter failed_clause.clause ~f:(fun literal ->
    let var = literal_var t ~literal in
    match var.assignment with
    | Null -> ()
    | This _ ->
      let trail_idx = trail_index_of_var_exn t ~var:(Int.abs literal) in
      keep := Int.max !keep (trail_idx + 1));
  !keep
;;

let pop_trail_after_conflict t ~failed_clause_idx =
  let keep_trail_len = conflict_keep_trail_len t ~failed_clause_idx in
  while Trail_entry.Vec.length t.trail > keep_trail_len do
    pop_from_trail_exn t
  done;
  t.trail_processed_till
  <- Int.min t.trail_processed_till (Trail_entry.Vec.length t.trail)
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
    (match t.theory with
     | Null -> ()
     | This (T ((module Theory), theory)) ->
       Theory.on_new_var theory ~var:(Int.abs literal);
       t.theory_reported_consistent <- false);
    Vsids.on_new_var t.vsids ~var:(Int.abs literal);
    var.exists <- true)
;;

let add_clause t ~literals ~origin =
  let len = Vec.Value.length literals in
  if len = 0 then t.has_empty_clause <- true;
  let satisfied = stack_ (ref false) in
  let num_unassigned = stack_ (ref 0) in
  let satisfied_at_front = stack_ (ref 0) in
  (* figure out if satisfied, and place unassigned variables at the front if not
     satisfied, and satisfied variables at the front if satisfied *)
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
         if !num_unassigned <= 1 && not !satisfied
         then Vec.Value.swap literals !num_unassigned i;
         incr num_unassigned);
      go (i + 1))
  in
  go 0;
  (* has unit is populated when the trail entry is seen *)
  let clause : Clause.t =
    { clause = literals
    ; has_unit = false
    ; origin
    ; lbd = 0
    ; deleted = false
    ; activity = 0.0
    }
  in
  let clause_idx = Vec.Value.length t.clauses in
  Vec.Value.push t.clauses clause;
  if len >= 2 then register_watchers_for_new_clause t ~literals ~clause_idx;
  if (not !satisfied) && !num_unassigned = 0 && len > 0
  then `Conflict clause_idx
  else (
    match !num_unassigned = 1 && not !satisfied with
    | false -> `Ok
    | true ->
      (* add unit *)
      (match
         push_unit_trail_entry t ~literal:(Vec.Value.get literals 0) ~clause_idx
       with
       | Null -> `Ok
       | This _ -> `Conflict clause_idx))
;;

let clause_act_decay_factor = 0.95
let clause_act_rescale_threshold = 1e20
let clause_act_rescale_divisor = 1e20

let add_clause_activity t ~clause_idx =
  let clause = Vec.Value.get t.clauses clause_idx in
  let new_act = clause.activity +. t.clause_act_inc in
  clause.activity <- new_act;
  Float.(new_act > clause_act_rescale_threshold)
;;

let rescale_clause_activities t =
  Vec.Value.iter t.clauses ~f:(fun clause ->
    clause.activity <- clause.activity /. clause_act_rescale_divisor);
  t.clause_act_inc <- t.clause_act_inc /. clause_act_rescale_divisor
;;

let decay_clause_activities t =
  t.clause_act_inc <- t.clause_act_inc /. clause_act_decay_factor
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
         if dl = 0 && t.decision_level <> 0
         then ()
         else (
           Vsids.add_activity t.vsids ~literal;
           if dl = t.decision_level
           then incr path_count
           else Vec.Value.push learned_literals literal)))
;;

let propagate_theory t = exclave_
  match t.theory with
  | Null -> `Consistent
  | This (T ((module Theory), theory)) ->
    (match Theory.maybe_get_lemma theory with
     | `Consistent ->
       t.theory_reported_consistent <- true;
       `Consistent
     | `Lemma { global = clause } ->
       t.theory_reported_consistent <- false;
       let trail_length_before = Trail_entry.Vec.length t.trail in
       (match
          add_clause
            t
            ~literals:(Vec.Value.of_array_taking_ownership clause)
            ~origin:Theory
        with
        | `Ok ->
          (* A non-unit branch-and-bound split can be re-derived until one of
             its literals is assigned, so make a decision before polling again.
             If no decision is available, [theory_reported_consistent] prevents
             [make_decision] from declaring Sat and the theory is polled again. *)
          if Trail_entry.Vec.length t.trail > trail_length_before
          then `Continue
          else `Consistent
        | `Conflict _ as res -> res))
;;

let rec propagate' t : int or_null =
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
       propagate' t)
;;

let rec propagate t =
  match propagate' t with
  | This _ as res -> res
  | Null ->
    (match propagate_theory t with
     | `Conflict clause_idx -> This clause_idx
     | `Consistent -> Null
     | `Continue -> propagate t)
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

let analyze_conflict t ~(failed_clause : Clause.t) ~failed_clause_idx =
  let seen = t.analyze_conflict_stamp_set in
  Stamp_set.reset seen;
  let learned_literals = t.analyze_conflict_scratch_literals in
  Vec.Value.clear learned_literals;
  let path_count = stack_ (ref 0) in
  let needs_rescale = ref false in
  let mark_literal literal =
    mark_literal t ~seen ~literal ~learned_literals ~path_count
  in
  needs_rescale
  := add_clause_activity t ~clause_idx:failed_clause_idx || !needs_rescale;
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
  match !path_count = 0 && Vec.Value.length learned_literals > 0 with
  | true ->
    (* No literal of the failed clause sits at the current decision level --
       e.g. a freshly-added unit clause directly conflicts with an
       assumption-decision from an earlier level (possible when multiple
       assumptions each occupy their own decision level). There is no 1UIP at
       [t.decision_level] to search for: every falsified literal already
       collected into [learned_literals] comes from a strictly lower decision
       level. Treat the one with the highest decision level as the asserting
       literal (its negation becomes the learned unit/clause's UIP), so
       backjumping lands just below it. *)
    if !needs_rescale then rescale_clause_activities t;
    let best_idx = ref 0 in
    let best_dl = ref (-1) in
    Vec.Value.iteri learned_literals ~f:(fun i literal ->
      let var = literal_var t ~literal in
      match%optional_u (var.trail_entry : Trail_entry.Option_u.t) with
      | None -> ()
      | Some trail_entry ->
        let dl = trail_entry.#decision_level in
        if dl > !best_dl
        then (
          best_dl := dl;
          best_idx := i));
    let uip_trail_literal = Vec.Value.get learned_literals !best_idx in
    let uip_literal = -uip_trail_literal in
    Vec.Value.swap learned_literals 0 !best_idx;
    Vec.Value.set learned_literals 0 uip_literal;
    simplify_learned_clause ~learned_literals ~uip_literal ~seen t
  | false ->
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
            needs_rescale := add_clause_activity t ~clause_idx || !needs_rescale;
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
    if !needs_rescale then rescale_clause_activities t;
    let uip_literal = !uip_literal in
    if Vec.Value.length learned_literals = 0
    then Vec.Value.push learned_literals uip_literal
    else (
      Vec.Value.push learned_literals (Vec.Value.get learned_literals 0);
      Vec.Value.set learned_literals 0 uip_literal);
    simplify_learned_clause ~learned_literals ~uip_literal ~seen t
;;

let rec luby_value i =
  let rec find_k k = if (1 lsl k) - 1 < i then find_k (k + 1) else k in
  let k = find_k 1 in
  if i = (1 lsl k) - 1
  then 1 lsl (k - 1)
  else (
    let prev_block = (1 lsl (k - 1)) - 1 in
    luby_value (i - prev_block))
;;

let compute_lbd t ~literals =
  let seen = t.lbd_stamp_set in
  Stamp_set.reset seen;
  let lbd = ref 0 in
  Vec.Value.iter literals ~f:(fun literal ->
    let var = literal_var t ~literal in
    match%optional_u (var.trail_entry : Trail_entry.Option_u.t) with
    | None -> ()
    | Some trail_entry ->
      let dl = trail_entry.#decision_level in
      if not (Stamp_set.is_seen seen ~var:dl)
      then (
        Stamp_set.mark_seen seen ~var:dl;
        incr lbd));
  !lbd
;;

let reduce_db t =
  let n = Vec.Value.length t.clauses in
  let candidates =
    Array.init n ~f:(fun i ->
      let c = Vec.Value.get t.clauses i in
      if (match c.Clause.origin with
          | Clause.Origin.Learned -> true
          | User | Theory -> false)
         && c.lbd >= 4
         && Vec.Value.length c.clause > 3
         && (not c.deleted)
         && not c.has_unit
      then Some (i, c.activity)
      else None)
    |> Array.filter_opt
  in
  (* sort ascending by activity: lowest activity deleted first *)
  Array.sort candidates ~compare:(fun (_, a) (_, b) -> Float.compare a b);
  let n_delete = Array.length candidates / 2 in
  for i = 0 to n_delete - 1 do
    let clause_idx, _ = candidates.(i) in
    let clause = Vec.Value.get t.clauses clause_idx in
    clause.deleted <- true
  done;
  Vec.Value.iter t.vars ~f:(fun (var : Var.t) ->
    if var.exists
    then (
      let filter_wl wl =
        Watched_clause.Vec.filter_inplace wl ~f:(fun wc ->
          not (Vec.Value.get t.clauses wc.#clause_idx).deleted)
      in
      filter_wl (Tf_pair.get var.watched_clauses true);
      filter_wl (Tf_pair.get var.watched_clauses false)));
  t.stats
  <- #{ t.stats with deleted_clauses = t.stats.#deleted_clauses + n_delete }
;;

let restart t = exclave_
  t.decision_level <- 0;
  while
    Trail_entry.Vec.length t.trail <> 0
    && (Trail_entry.Vec.last_exn t.trail).#decision_level
       > t.decision_level_of_last_assumption
  do
    pop_from_trail_exn t
  done;
  t.trail_processed_till
  <- Int.min t.trail_processed_till (Trail_entry.Vec.length t.trail)
;;

let backtrack t ~failed_clause ~failed_clause_idx =
  let #(~learned_literals, ~backjump_level) =
    analyze_conflict t ~failed_clause ~failed_clause_idx
  in
  let lbd = compute_lbd t ~literals:learned_literals in
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
  t.conflicts_since_restart <- t.conflicts_since_restart + 1;
  Vsids.decay t.vsids;
  remove_greater_than_decision_level t ~decision_level:backjump_level;
  let result = add_clause t ~literals:learned_literals ~origin:Learned in
  (match result with
   | `Ok ->
     let clause = Vec.Value.get t.clauses (Vec.Value.length t.clauses - 1) in
     clause.lbd <- lbd;
     clause.activity <- t.clause_act_inc
   | `Conflict _ -> ());
  let luby_threshold = 32 * luby_value t.luby_index in
  if t.conflicts_since_restart >= luby_threshold
     && t.decision_level > t.decision_level_of_last_assumption
  then (
    t.stats <- #{ t.stats with restarts = t.stats.#restarts + 1 };
    t.conflicts_since_restart <- 0;
    t.luby_index <- t.luby_index + 1;
    restart t);
  result
;;

(* Traverses the reason graph from [failed_clause_idx] and collects every User
   and Theory clause that transitively caused the conflict. Learned (CDCL)
   clauses are transparent: we recurse into their literals' reasons instead. For
   User/Theory clauses we also recurse so we reach the User clauses that made
   each Theory clause's literals false. *)
let extract_unsat_core t failed_clause_idx : Sat_result.Core_clause.t list =
  let seen_vars = Stamp_set.create () in
  Stamp_set.reset seen_vars;
  let seen_clause_idxs = Hash_set.create (module Core.Int) in
  let result = Vec.Value.create () in
  let rec explore clause_idx =
    if not (Hash_set.mem seen_clause_idxs clause_idx)
    then (
      Hash_set.add seen_clause_idxs clause_idx;
      let clause = Vec.Value.get t.clauses clause_idx in
      (match clause.origin with
       | User | Theory ->
         Vec.Value.push
           result
           { Sat_result.Core_clause.literals = Vec.Value.to_array clause.clause
           ; is_theory =
               (match clause.origin with
                | Theory -> true
                | User | Learned -> false)
           }
       | Learned -> ());
      Vec.Value.iter clause.clause ~f:(fun literal ->
        let var = Int.abs literal in
        if not (Stamp_set.is_seen seen_vars ~var)
        then (
          Stamp_set.mark_seen seen_vars ~var;
          let var_obj = literal_var t ~literal in
          match%optional_u (var_obj.trail_entry : Trail_entry.Option_u.t) with
          | None -> ()
          | Some trail_entry ->
            (match trail_entry.#reason with
             | T #(Decision, ()) -> ()
             | T #(Clause_idx, reason_clause_idx) -> explore reason_clause_idx))))
  in
  explore failed_clause_idx;
  Vec.Value.to_list result
;;

let%template unsat t failed_clause_idx : Sat_result.t @ m =
  let core = extract_unsat_core t failed_clause_idx in
  Unsat { core } [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let make_decision' ~is_assumption t ~literal =
  if t.debug
  then print_s [%message "make_decision" (is_assumption : bool) (literal : int)];
  t.decision_level <- t.decision_level + 1;
  if not is_assumption
  then
    t.stats
    <- #{ t.stats with
          decisions = t.stats.#decisions + 1
        ; max_decision_level =
            Int.max t.decision_level t.stats.#max_decision_level
        }
  else
    t.stats
    <- #{ t.stats with
          max_decision_level =
            Int.max t.decision_level t.stats.#max_decision_level
        };
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
  match Vsids.choose_literal t.vsids with
  | Null ->
    if t.theory_reported_consistent
    then `Done (Sat_result.Sat { assignments = assignments_array t })
    else `Continue
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
    if t.debug
    then
      Vec.Value.iter t.clauses ~f:(fun clause ->
        if not clause.deleted
        then (
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
                  (clause.clause : int Vec.Value.t)]));
    sat_result
[@@alloc a @ m = (stack_local, heap_global)]
;;

let simplify_clauses_every = 2500

let%template rec solve' t ~timer : Sat_result.t @ m =
  (t.stats <- #{ t.stats with iterations = t.stats.#iterations + 1 };
   t.iterations <- t.iterations + 1;
   Timer.check timer;
   if t.iterations mod simplify_clauses_every = 0
   then (
     reduce_db t;
     decay_clause_activities t);
   match propagate t with
   | This failed_clause_idx -> learn_from_failure t ~failed_clause_idx ~timer
   | Null ->
     (match (make_decision [@alloc a]) t with
      | `Continue -> (solve' [@alloc a]) t ~timer
      | `Failed_clause failed_clause_idx ->
        (learn_from_failure [@alloc a]) t ~failed_clause_idx ~timer
      | `Done sat_result -> (check_sat_result [@alloc a]) t ~sat_result))
  [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]

and learn_from_failure t ~failed_clause_idx ~timer : Sat_result.t @ m =
  pop_trail_after_conflict t ~failed_clause_idx;
  match[@exclave_if_stack a]
    t.decision_level = 0
    || t.decision_level <= t.decision_level_of_last_assumption
  with
  | true -> (unsat [@alloc a]) t failed_clause_idx
  | false ->
    let failed_clause = Vec.Value.get t.clauses failed_clause_idx in
    t.stats <- #{ t.stats with conflicts = t.stats.#conflicts + 1 };
    (match backtrack t ~failed_clause ~failed_clause_idx with
     | `Ok -> (solve' [@alloc a]) t ~timer
     | `Conflict failed_clause_idx ->
       (learn_from_failure [@alloc a]) t ~failed_clause_idx ~timer)
[@@alloc a @ m = (stack_local, heap_global)]
;;

let add_assumptions ~(local_ assumptions) t ~timer = exclave_
  let rec go i = exclave_
    if i = Array.length assumptions
    then `Continue
    else (
      t.stats <- #{ t.stats with iterations = t.stats.#iterations + 1 };
      Timer.check timer;
      let literal = assumptions.(i) in
      ensure_literal t ~literal;
      let var = literal_var t ~literal in
      match var.assignment with
      | Null ->
        (match make_decision' ~is_assumption:true t ~literal with
         | This failed_clause_idx -> `Failed_clause failed_clause_idx
         | Null -> go (i + 1))
      | This b ->
        if Bool.equal b (literal > 0)
        then go (i + 1)
        else (
          let trail_entry = Trail_entry.Option_u.value_exn var.trail_entry in
          match trail_entry.#reason with
          | T #(Decision, ()) ->
            `Failed_assumptions (trail_entry.#literal, literal)
          | T #(Clause_idx, failed_clause_idx) ->
            `Failed_clause failed_clause_idx))
  in
  go 0
;;

let maybe_clear_past_solve_state t =
  let has_run_before = t.stats.#iterations > 0 in
  t.stats <- Stats.empty ();
  t.luby_index <- 1;
  t.conflicts_since_restart <- 0;
  t.iterations <- 0;
  t.clause_act_inc <- 1.0;
  if has_run_before
  then (
    if t.debug then print_endline "restarting";
    (* Between [solve] calls there are no active assumptions: reset to 0 so
       [restart] pops the trail all the way back to decision level 0, rather
       than leaving behind decision-level-1 (etc.) entries from the previous
       [solve]'s assumptions. Otherwise those stale entries would coexist with
       the next [solve]'s own (unrelated) decision-level-1 entries, breaking the
       1UIP invariant in [analyze_conflict]. *)
    t.decision_level_of_last_assumption <- 0;
    restart t)
;;

let%template solve ?(time_bound = `Unlimited) ?(local_ assumptions = [||]) t
  : Sat_result.t @ m
  =
  t.decision_level_of_last_assumption <- 0;
  let timer = Timer.create time_bound in
  if t.debug
  then
    print_s
      [%message
        "solve" ~assumptions:([%globalize: int array] assumptions : int array)];
  maybe_clear_past_solve_state t;
  if t.has_empty_clause
  then Unsat { core = [] }
  else (
    match[@exclave_if_stack a] add_assumptions ~assumptions t ~timer with
    | `Continue -> (solve' [@alloc a]) t ~timer
    | `Failed_clause failed_clause_idx ->
      pop_trail_after_conflict t ~failed_clause_idx;
      (unsat [@alloc a]) t failed_clause_idx
    | `Failed_assumptions (previous_assumption, assumption) ->
      Unsat
        { core =
            [ { Sat_result.Core_clause.literals =
                  [| previous_assumption; assumption |]
              ; is_theory = false
              }
            ]
        })
[@@alloc a @ m = (stack_local, heap_global)]
;;

let create
  ?theory
  ?(random_state = Random.State.make [| 1; 2; 3 |])
  ?(debug = false)
  ()
  =
  let _ = random_state in
  { trail = Trail_entry.Vec.create ()
  ; trail_processed_till = 0
  ; decision_level = 0
  ; decision_level_of_last_assumption = 0
  ; clauses = Vec.Value.create ()
  ; vars = Vec.Value.create ()
  ; has_empty_clause = false
  ; stats = Stats.empty ()
  ; analyze_conflict_stamp_set = Stamp_set.create ()
  ; analyze_conflict_scratch_literals = Vec.Value.create ()
  ; debug
  ; vsids = Vsids.create ()
  ; luby_index = 1
  ; conflicts_since_restart = 0
  ; lbd_stamp_set = Stamp_set.create ()
  ; clause_act_inc = 1.0
  ; iterations = 0
  ; theory =
      (match theory with
       | None -> Null
       | Some packed -> This packed)
  ; theory_reported_consistent = Option.is_none theory
  }
;;

let add_clause t ~clause =
  maybe_clear_past_solve_state t;
  match
    add_clause
      t
      ~literals:(Vec.Value.of_array_taking_ownership clause)
      ~origin:User
  with
  | `Ok -> `Ok
  | `Conflict failed_clause_idx ->
    `Unsat (extract_unsat_core t failed_clause_idx)
;;

let create_with_formula ?theory ?(local_ debug) formula =
  let t = create ?theory ?debug () in
  let core = ref None in
  Array.iter formula ~f:(fun clause ->
    match !core with
    | Some _ -> ()
    | None ->
      (match add_clause t ~clause with
       | `Ok -> ()
       | `Unsat p -> core := Some p));
  match !core with
  | None -> `Ok t
  | Some p -> `Unsat p
;;
