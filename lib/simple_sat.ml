open! Core
open! Ds

module Stats = struct
  module Profile = struct
    module Bucket = struct
      type t =
        { count : int
        ; elapsed_ns : float
        }
      [@@deriving sexp]
    end

    type t =
      { add_clause : Bucket.t
      ; select_literal : Bucket.t
      ; unit_propagate : Bucket.t
      ; backtrack : Bucket.t
      }
    [@@deriving sexp]
  end

  type t =
    { decisions : int
    ; conflicts : int
    ; learned : int
    ; learned_clause_literals : int
    ; assignments : int
    ; profile : Profile.t option
    }
  [@@deriving sexp]
end

module Profile_state = struct
  type bucket =
    { mutable count : int
    ; mutable elapsed_ns : float
    }

  type t =
    { add_clause : bucket
    ; select_literal : bucket
    ; unit_propagate : bucket
    ; backtrack : bucket
    }

  let bucket () = { count = 0; elapsed_ns = 0. }

  let create () =
    { add_clause = bucket ()
    ; select_literal = bucket ()
    ; unit_propagate = bucket ()
    ; backtrack = bucket ()
    }
  ;;

  let record bucket start =
    bucket.count <- bucket.count + 1;
    bucket.elapsed_ns
    <- bucket.elapsed_ns
       +. (Time_ns.diff (Time_ns.now ()) start |> Time_ns.Span.to_ns)
  ;;

  let snapshot_bucket ({ count; elapsed_ns } : bucket) =
    Stats.Profile.Bucket.{ count; elapsed_ns }
  ;;

  let snapshot (t : t) =
    Stats.Profile.
      { add_clause = snapshot_bucket t.add_clause
      ; select_literal = snapshot_bucket t.select_literal
      ; unit_propagate = snapshot_bucket t.unit_propagate
      ; backtrack = snapshot_bucket t.backtrack
      }
  ;;
end

type clause = int array

type variable =
  { mutable set : bool
  ; mutable sign : bool
  ; mutable mark : bool
  ; mutable unit_ : bool
  ; mutable unit_sign : bool
  ; mutable dlevel : int
  ; mutable reason : clause option
  ; watches : clause Vec.Value.t array
  }

type state =
  { mutable empty : bool
  ; vars : variable array
  ; trail : int Vec.Value.t
  ; mutable dlevel : int
  ; mutable decisions : int
  ; mutable conflicts : int
  ; mutable learned : int
  ; mutable learned_clause_literals : int
  ; mutable assignments : int
  ; profile : Profile_state.t option
  }

let profile_start state = Option.map state.profile ~f:(fun _ -> Time_ns.now ())

let profile_record profile start ~f =
  match profile, start with
  | Some profile, Some start -> Profile_state.record (f profile) start
  | _ -> ()
;;

let create_variable () =
  { set = false
  ; sign = false
  ; mark = false
  ; unit_ = false
  ; unit_sign = false
  ; dlevel = 0
  ; reason = None
  ; watches = [| Vec.Value.create (); Vec.Value.create () |]
  }
;;

let literal_get_idx literal = Int.abs literal
let literal_get_sign literal = literal < 0
let literal_get_var state literal = state.vars.(literal_get_idx literal)

let literal_is_false state literal =
  let v = literal_get_var state literal in
  v.set && not (Bool.equal v.sign (literal_get_sign literal))
;;

let literal_get_mark state literal =
  let v = literal_get_var state literal in
  v.mark
;;

let literal_add_watch state literal clause =
  let v = literal_get_var state literal in
  let watch = v.watches.(Bool.to_int (literal_get_sign literal)) in
  Vec.Value.push watch clause
;;

let literal_set state literal reason =
  let v = literal_get_var state literal in
  v.sign <- literal_get_sign literal;
  v.set <- true;
  v.dlevel <- state.dlevel;
  v.reason <- reason;
  state.assignments <- state.assignments + 1;
  Vec.Value.push state.trail literal
;;

let sat_add_clause state clause =
  let profile = profile_start state in
  match Array.length clause with
  | 0 -> state.empty <- true
  | 1 ->
    let v = literal_get_var state clause.(0) in
    let sign = literal_get_sign clause.(0) in
    if v.unit_
    then (if not (Bool.equal sign v.unit_sign) then state.empty <- true)
    else (
      v.unit_ <- true;
      v.unit_sign <- sign)
  | _ ->
    literal_add_watch state clause.(0) clause;
    literal_add_watch state clause.(1) clause;
    profile_record state.profile profile ~f:(fun p -> p.add_clause)
;;

let sat_select_literal state =
  let profile = profile_start state in
  let n = Array.length state.vars - 1 in
  if n <= 0
  then (
    profile_record state.profile profile ~f:(fun p -> p.select_literal);
    0)
  else (
    let start = 1 + Stdlib.Random.int n in
    let i = ref start in
    let found = ref None in
    while Option.is_none !found do
      if not state.vars.(!i).set
      then found := Some !i
      else (
        incr i;
        if !i >= Array.length state.vars then i := 1;
        if !i = start then found := Some 0)
    done;
    let result =
      match !found with
      | Some 0 -> 0
      | Some idx -> if Stdlib.Random.bool () then -idx else idx
      | None -> assert false
    in
    if result <> 0 then state.decisions <- state.decisions + 1;
    profile_record state.profile profile ~f:(fun p -> p.select_literal);
    result)
;;

let remove_watch_at watch idx =
  let last_slot = Vec.Value.length watch - 1 in
  if idx <> last_slot
  then Vec.Value.set watch idx (Vec.Value.get watch last_slot);
  ignore (Vec.Value.pop_exn watch : clause)
;;

let rec sat_backtrack state reason =
  let profile = profile_start state in
  if state.dlevel = 0
  then (
    profile_record state.profile profile ~f:(fun p -> p.backtrack);
    None)
  else (
    state.conflicts <- state.conflicts + 1;
    let conflicts = Vec.Value.create () in
    let count = ref 0 in
    Array.iter reason ~f:(fun literal ->
      let v = literal_get_var state literal in
      if v.dlevel <> 0
      then (
        v.mark <- true;
        if v.dlevel < state.dlevel
        then Vec.Value.push conflicts literal
        else incr count));
    let tlevel = ref (Vec.Value.length state.trail - 1) in
    let literal = ref 0 in
    let found_uip = ref false in
    while not !found_uip do
      if !tlevel < 0
      then (
        found_uip := true;
        literal := 0)
      else (
        let curr_literal = Vec.Value.get state.trail !tlevel in
        decr tlevel;
        let v = literal_get_var state curr_literal in
        v.set <- false;
        if v.mark
        then (
          v.mark <- false;
          decr count;
          if !count <= 0
          then (
            literal := curr_literal;
            found_uip := true)
          else (
            match v.reason with
            | None -> ()
            | Some reason_clause ->
              for i = 1 to Array.length reason_clause - 1 do
                let reason_literal = reason_clause.(i) in
                let w = literal_get_var state reason_literal in
                if (not w.mark) && w.dlevel <> 0
                then (
                  if w.dlevel < state.dlevel
                  then Vec.Value.push conflicts reason_literal
                  else incr count;
                  w.mark <- true)
              done)))
    done;
    if !literal = 0
    then None
    else (
      let nogood = Vec.Value.singleton (- !literal) in
      let blevel = ref 0 in
      Vec.Value.iter conflicts ~f:(fun conflict_literal ->
        let v = literal_get_var state conflict_literal in
        let keep_literal =
          match v.reason with
          | None -> true
          | Some reason_clause ->
            let k = ref 1 in
            while
              !k < Array.length reason_clause
              && literal_get_mark state reason_clause.(!k)
            do
              incr k
            done;
            !k < Array.length reason_clause
        in
        if keep_literal
        then (
          Vec.Value.push nogood conflict_literal;
          if !blevel < v.dlevel
          then (
            blevel := v.dlevel;
            let last_idx = Vec.Value.length nogood - 1 in
            let second = Vec.Value.get nogood 1 in
            Vec.Value.set nogood last_idx second;
            Vec.Value.set nogood 1 conflict_literal)));
      let retain_length = ref 0 in
      while !tlevel >= 0 && !retain_length = 0 do
        let trail_literal = Vec.Value.get state.trail !tlevel in
        let v = literal_get_var state trail_literal in
        if v.dlevel <= !blevel
        then retain_length := !tlevel + 1
        else (
          v.set <- false;
          decr tlevel)
      done;
      while Vec.Value.length state.trail > !retain_length do
        ignore (Vec.Value.pop_exn state.trail : int)
      done;
      Vec.Value.iter conflicts ~f:(fun conflict_literal ->
        let v = literal_get_var state conflict_literal in
        v.mark <- false);
      let nogood = Vec.Value.to_array nogood in
      state.learned <- state.learned + 1;
      state.learned_clause_literals
      <- state.learned_clause_literals + Array.length nogood;
      sat_add_clause state nogood;
      state.dlevel <- !blevel;
      profile_record state.profile profile ~f:(fun p -> p.backtrack);
      if state.empty then None else Some nogood))

and sat_unit_propagate state literal reason =
  let profile = profile_start state in
  let literal = ref literal in
  let reason = ref reason in
  let restart = ref false in
  let result = ref true in
  let continue_outer = ref true in
  while !continue_outer do
    let curr = ref (Vec.Value.length state.trail) in
    let next = ref (!curr + 1) in
    literal_set state !literal !reason;
    restart := false;
    while !curr < !next && not !restart do
      let propagated_literal = -Vec.Value.get state.trail !curr in
      incr curr;
      let v = literal_get_var state propagated_literal in
      let watch =
        v.watches.(Bool.to_int (literal_get_sign propagated_literal))
      in
      let i = ref 0 in
      while !i < Vec.Value.length watch && not !restart do
        let clause = Vec.Value.get watch !i in
        let watch_idx = if clause.(0) = propagated_literal then 1 else 0 in
        let watch_literal = clause.(watch_idx) in
        let watch_sign = literal_get_sign watch_literal in
        let w = literal_get_var state watch_literal in
        if w.set && Bool.equal w.sign watch_sign
        then incr i
        else (
          let j = ref 2 in
          while
            !j < Array.length clause && literal_is_false state clause.(!j)
          do
            incr j
          done;
          if !j >= Array.length clause
          then
            if not w.set
            then (
              if watch_idx <> 0
              then (
                clause.(0) <- watch_literal;
                clause.(1) <- propagated_literal);
              literal_set state watch_literal (Some clause);
              incr next;
              incr i)
            else (
              reason := sat_backtrack state clause;
              match !reason with
              | None ->
                result := false;
                continue_outer := false;
                restart := true
              | Some learned_clause ->
                literal := learned_clause.(0);
                restart := true)
          else (
            let new_literal = clause.(!j) in
            clause.(1 - watch_idx) <- new_literal;
            clause.(!j) <- propagated_literal;
            literal_add_watch state new_literal clause;
            remove_watch_at watch !i))
      done
    done;
    if not !restart then continue_outer := false
  done;
  let result = !result in
  profile_record state.profile profile ~f:(fun p -> p.unit_propagate);
  result
;;

let stats (state : state) =
  Stats.
    { decisions = state.decisions
    ; conflicts = state.conflicts
    ; learned = state.learned
    ; learned_clause_literals = state.learned_clause_literals
    ; assignments = state.assignments
    ; profile = Option.map state.profile ~f:Profile_state.snapshot
    }
;;

let solve_with_stats ?(profile = false) ~size clauses =
  let state =
    { empty = false
    ; vars = Array.init (size + 1) ~f:(fun _ -> create_variable ())
    ; trail = Vec.Value.create ()
    ; dlevel = 0
    ; decisions = 0
    ; conflicts = 0
    ; learned = 0
    ; learned_clause_literals = 0
    ; assignments = 0
    ; profile = (if profile then Some (Profile_state.create ()) else None)
    }
  in
  Array.iter clauses ~f:(fun clause -> sat_add_clause state (Array.copy clause));
  let result =
    if state.empty
    then false
    else (
      let solved = ref true in
      let i = ref 1 in
      while !solved && !i < Array.length state.vars do
        let v = state.vars.(!i) in
        if v.unit_
        then (
          let literal = if v.unit_sign then - !i else !i in
          solved := sat_unit_propagate state literal None);
        incr i
      done;
      if not !solved
      then false
      else (
        let continue = ref true in
        state.dlevel <- 1;
        while !continue do
          let literal = sat_select_literal state in
          if literal = 0
          then continue := false
          else if not (sat_unit_propagate state literal None)
          then (
            solved := false;
            continue := false)
          else state.dlevel <- state.dlevel + 1
        done;
        !solved))
  in
  result, stats state
;;

let solve ?profile ~size clauses = fst (solve_with_stats ?profile ~size clauses)

let dimacs_size dimacs clauses =
  let parse_header line =
    let pieces =
      String.split ~on:' ' (String.strip line)
      |> List.filter ~f:(Fn.non String.is_empty)
    in
    match pieces with
    | [ "p"; "cnf"; size; _num_clauses ] -> Int.of_string_opt size
    | _ -> None
  in
  match List.find_map (String.split_lines dimacs) ~f:parse_header with
  | Some size -> size
  | None ->
    Array.fold clauses ~init:0 ~f:(fun acc clause ->
      Array.fold clause ~init:acc ~f:(fun acc literal ->
        Int.max acc (Int.abs literal)))
;;

let dimacs_clauses dimacs =
  String.split_lines dimacs
  |> List.filter_map ~f:(fun line ->
    let line = String.strip line in
    if String.is_empty line
       || Char.equal line.[0] 'c'
       || Char.equal line.[0] 'p'
    then None
    else (
      let literals =
        String.split ~on:' ' line
        |> List.filter_map ~f:(fun token ->
          match String.strip token with
          | "" -> None
          | token -> Int.of_string_opt token)
        |> List.filter ~f:(fun literal -> literal <> 0)
      in
      if List.is_empty literals then None else Some (Array.of_list literals)))
  |> Array.of_list
;;

let solve_dimacs_string_with_stats ?profile dimacs =
  let clauses = dimacs_clauses dimacs in
  solve_with_stats ?profile ~size:(dimacs_size dimacs clauses) clauses
;;

let solve_dimacs_string ?profile dimacs =
  fst (solve_dimacs_string_with_stats ?profile dimacs)
;;
