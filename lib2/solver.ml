open! Core
open! Import
open! Stdlib_stable

type t =
  { trail : Trail_entry.Vec.t
  ; mutable trail_processed_till : int
  ; mutable decision_level : int
  ; clauses : Clause.t Vec.Value.t
  ; vars : Var.t Vec.Value.t
  ; mutable has_empty_clause : bool
  ; mutable stats : Stats.t
  }

let push_unit_trail_entry t ~literal ~clause_idx =
  Trail_entry.Vec.push
    t.trail
    (#{ decision_level = t.decision_level
      ; literal
      ; reason = Reason.clause_idx clause_idx
      }
     : Trail_entry.t)
;;

let literal_var t ~literal = Vec.Value.get t.vars (Int.abs literal)

let is_satisfied t ~literal =
  let var = literal_var t ~literal in
  [%equal: bool or_null] var.assignment (This (literal > 0))
;;

let replace_watched_literal_for_non_binary_clause
  t
  ~clause_idx
  ~nullified_literal
  =
  let clause = Vec.Value.get t.clauses clause_idx in
  assert (Vec.Value.length clause.clause > 2);
  if Vec.Value.get clause.clause 0 = nullified_literal
  then (
    Vec.Value.set clause.clause 0 (Vec.Value.get clause.clause 1);
    Vec.Value.set clause.clause 1 nullified_literal);
  let other_literal = Vec.Value.get clause.clause 0 in
  let other_var = literal_var t ~literal:other_literal in
  if is_satisfied t ~literal:other_literal
  then (* already satisfied, do nothing *)
    `Already_satisfied
  else if not (Or_null.is_null other_var.assignment)
  then
    (* other watched literal is already assigned, so there can't be a replacement, so this is a conflict *)
    `No_replacement
  else (
    let rec go i = exclave_
      if i >= Vec.Value.length clause.clause
      then `No_candidate_replacement
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
            `Replacement (~var:{ global = var }, ~literal, ~i)))
    in
    match go 2 with
    | `Already_satisfied -> `Already_satisfied
    | `No_candidate_replacement ->
      (* other watched literal is a unit *)
      push_unit_trail_entry t ~literal:other_literal ~clause_idx;
      `Found_unit
    | `Replacement (~var:{ global = var }, ~literal, ~i) ->
      Vec.Value.set clause.clause i nullified_literal;
      Vec.Value.set clause.clause 1 literal;
      (* guaranteed non binary *)
      Watched_clause.Vec.push
        (Tf_pair.get var.watched_clauses (literal > 0))
        (Watched_clause.create
           ~clause_idx
           ~blocking_literal:other_literal
           ~is_binary:false);
      `Replaced)
;;

let update_watches_after_assignment t ~(var : Var.t) ~literal =
  let assignment = Or_null.get var.assignment in
  (* other assignment invalidated *)
  let watched_clauses = Tf_pair.get var.watched_clauses (not assignment) in
  Watched_clause.Vec.filter_inplace
    watched_clauses
    ~f:(fun #{ clause_idx; blocking_literal; is_binary } ->
      if is_satisfied t ~literal:blocking_literal
      then true (* already satisfied, do nothing *)
      else if is_binary
      then (
        (* immediately add unit literal, because binary and this literal isn't satisfied *)
        push_unit_trail_entry t ~literal:blocking_literal ~clause_idx;
        true)
      else (
        (* TODO can have conflict here if no replacement and not unit/satisfied *)
        match
          replace_watched_literal_for_non_binary_clause
            t
            ~clause_idx
            ~nullified_literal:literal
        with
        | `Replaced -> false
        | `Not_replaced -> true))
  [@nontail]
;;

let rec propagate t : int or_null =
  match t.trail_processed_till >= Trail_entry.Vec.length t.trail with
  | true -> Null
  | false ->
    let trail_entry = Trail_entry.Vec.get t.trail t.trail_processed_till in
    t.trail_processed_till <- t.trail_processed_till + 1;
    let value = trail_entry.#literal > 0 in
    let var = Vec.Value.get t.vars (Int.abs trail_entry.#literal) in
    (match var.assignment with
     | This value' when Bool.equal value value' -> propagate t
     | This _ ->
       (match trail_entry.#reason with
        | T #(Decision, ()) ->
          failwith "propagate: BUG conflicting assignment from decision"
        | T #(Clause_idx, clause_idx) -> This clause_idx)
     | Null ->
       var.assignment <- This value;
       var.trail_entry <- Trail_entry.Option_u.some trail_entry;
       (match trail_entry.#reason with
        | T #(Decision, ()) -> ()
        | T #(Clause_idx, clause_idx) ->
          (Vec.Value.get t.clauses clause_idx).has_unit <- true);
       t.stats <- #{ t.stats with propagations = t.stats.#propagations + 1 };
       update_watches_after_assignment t ~var ~literal:trail_entry.#literal;
       propagate t)
;;

let register_watcher t ~literal ~clause_idx =
  Watched_clause.Vec.push
    (Tf_pair.get var.watched_clauses (literal > 0))
    (Watched_clause.create
       ~clause_idx
       ~blocking_literal:other_literal
       ~is_binary:false)
;;

let add_clause t ~literals =
  let len = Vec.Value.length literals in
  if len = 0 then t.has_empty_clause <- true;
  let satisfied = stack_ (ref false) in
  let num_unassigned = stack_ (ref 0) in
  let rec go i =
    if i >= Vec.Value.length literals
    then ()
    else (
      let literal = Vec.Value.get literals i in
      let var = literal_var t ~literal in
      satisfied := is_satisfied t ~literal;
      if !satisfied
      then ()
      else (
        match var.assignment with
        | This _ -> ()
        | Null ->
          if !num_unassigned < 2 then Vec.Value.swap literals !num_unassigned i;
          incr num_unassigned))
  in
  go 0;
  let clause : Clause.t = { clause = literals; has_unit = false } in
  let clause_idx = Vec.Value.legnth t.clauses in
  Vec.Value.push t.clauses clause;
  if len >= 2
  then register_watcher t ~literal:(Vec.Value.get literals 0) ~clause_idx;
  if !satisfied
  then `Satisfied
  else if !num_unassigned = 1
  then `Unit
  else `Other
;;

let%template rec solve' t : Sat_result.t @ m = failwith "TODO"
[@@alloc a @ m = (stack_local, heap_global)]
;;
