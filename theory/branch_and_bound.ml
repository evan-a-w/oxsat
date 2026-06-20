open! Core
open! Feel.Import

module Tvar_and_type = struct
  type t =
    { tvar : Tvar.t
    ; type_ : Type_expr.Base.t
    }
  [@@deriving sexp, compare, hash]

  include functor Comparable.Make
  include functor Hashable.Make
end

module Trail_entry = struct
  module Kind = struct
    type t = Add_constraint of Simplex.constraint_
    (* | Make_integral_assignment of Tvar.t *)
    (* | Make_non_integral_assignment of Tvar.t *)
    (* | Relaxation of *)
    (*     { var : int *)
    (* ; lb : Simplex.constraint_ *)
    (* ; ub : Simplex.constraint_ *)
    (*     } *)
  end

  type t =
    { decision_level : int
    ; kind : Kind.t
    }
end

type t =
  { simplex_var_by_tvar_and_type : int Tvar_and_type.Table.t
  ; tvar_type_by_simplex_var : (Tvar.t * Type_expr.Base.t) Int.Table.t
  ; non_integral_ints : unit Tvar.Hash_queue.t
  ; simplex : Simplex.t
  ; trail : Trail_entry.t Vec.Value.t
  ; mutable current_decision_level : int
  }

let rec undo t ~to_decision_level_excl =
  match Vec.Value.length t.trail with
  | 0 -> ()
  | len ->
    let trail_entry = Vec.Value.get t.trail (len - 1) in
    (match trail_entry.decision_level > to_decision_level_excl with
     | false -> t.current_decision_level <- trail_entry.decision_level
     | true ->
       ignore (Vec.Value.pop_exn t.trail : Trail_entry.t);
       (match trail_entry.kind with
        | Add_constraint constraint_ ->
          Simplex.remove_constraint t.simplex ~constraint_
          (* | Make_integral_assignment tvar -> *)
          (* Hash_queue.enqueue_front_exn t.non_integral_ints tvar () *)
          (* | Make_non_integral_assignment tvar -> *)
          (* Hash_queue.remove_exn t.non_integral_ints tvar *)
          (* | Relaxation { var = _; lb; ub } -> *)
          (* Simplex.remove_constraint t.simplex ~constraint_:lb; *)
          (* Simplex.remove_constraint t.simplex ~constraint_:ub *));
       undo t ~to_decision_level_excl)
;;

let rec undo_to_length t ~length =
  if Vec.Value.length t.trail <= length
  then ()
  else (
    let trail_entry = Vec.Value.get t.trail (Vec.Value.length t.trail - 1) in
    ignore (Vec.Value.pop_exn t.trail : Trail_entry.t);
    (match trail_entry.kind with
     | Add_constraint constraint_ ->
       Simplex.remove_constraint t.simplex ~constraint_);
    undo_to_length t ~length)
;;

let push_trail t ~kind =
  Vec.Value.push
    t.trail
    ({ decision_level = t.current_decision_level; kind } : Trail_entry.t)
;;

let simplex_solve t =
  let res = Simplex.solve t.simplex in
  let new_assignments = Simplex.new_assignments t.simplex in
  let rec go () =
    match Hash_queue.dequeue_front_with_key new_assignments with
    | None -> ()
    | Some (var, q) ->
      (match Hashtbl.find t.tvar_type_by_simplex_var var with
       | None | Some (_, Float) -> ()
       | Some (tvar, Int) ->
         let marked_nonintegral = Hash_queue.mem t.non_integral_ints tvar in
         if marked_nonintegral && Q.is_integral q
         then
           Hash_queue.remove_exn
             t.non_integral_ints
             tvar (* push_trail t ~kind:(Make_integral_assignment tvar) *)
         else if (not marked_nonintegral) && not (Q.is_integral q)
         then
           Hash_queue.enqueue_front_exn
             t.non_integral_ints
             tvar
             () (* push_trail t ~kind:(Make_non_integral_assignment tvar) *));
      go ()
  in
  go ();
  res
;;

let rec solve t =
  let[@inline always] try_with_constraint constraint_ =
    let length = Vec.Value.length t.trail in
    let constraint_ = Simplex.add_constraint t.simplex constraint_ in
    push_trail t ~kind:(Add_constraint constraint_);
    let res = solve t in
    (match res with
     | `Unsat -> undo_to_length t ~length
     | `Sat -> ());
    res
  in
  match Simplex.solve t.simplex with
  | `Unsat -> `Unsat
  | `Sat ->
    (* relaxed problem is feasible *)
    (match Hash_queue.first_with_key t.non_integral_ints with
     | None -> `Sat
     | Some (nonintegral, ()) ->
       let var =
         Hashtbl.find_exn
           t.simplex_var_by_tvar_and_type
           { tvar = nonintegral; type_ = Int }
       in
       let assignment = Simplex.assignment t.simplex ~var in
       `Unsat)
;;
