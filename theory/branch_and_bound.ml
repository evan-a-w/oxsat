open! Core
open! Feel.Import

module Atom = struct
  type t =
    [ `Le of Linear_expr.t * Q.t
    | `Type_eq of Type_expr.t * Type_expr.t
    ]
  [@@deriving sexp, compare, hash]
end

module Trail_entry = struct
  module Kind = struct
    type t =
      | Add_constraint of Simplex.constraint_
      | Set_integral of
          { tvar : Tvar.t
          ; previous : bool
          }
  end

  type t =
    { decision_level : int
    ; kind : Kind.t
    }
end

type t =
  { simplex_var_by_tvar : int Tvar.Table.t
  ; tvar_by_simplex_var : Tvar.t Int.Table.t
  ; integral_tvars : Tvar.Hash_set.t
  ; non_integral_ints : unit Tvar.Hash_queue.t
  ; simplex : Simplex.t
  ; trail : Trail_entry.t Vec.Value.t
  ; mutable current_decision_level : int
  }

(* Brings [non_integral_ints] membership for [tvar] in line with whether it's
   currently in [integral_tvars] and, if so, whether its simplex assignment is
   integral. *)
let refresh_non_integral t ~tvar =
  match Hashtbl.find t.simplex_var_by_tvar tvar with
  | None -> ()
  | Some var ->
    let marked_nonintegral = Hash_queue.mem t.non_integral_ints tvar in
    let should_be_nonintegral =
      Hash_set.mem t.integral_tvars tvar
      && not (Simplex.Q_eps.is_integral (Simplex.assignment t.simplex ~var))
    in
    (match should_be_nonintegral with
     | true ->
       if not marked_nonintegral
       then Hash_queue.enqueue_front_exn t.non_integral_ints tvar ()
     | false ->
       if marked_nonintegral then Hash_queue.remove_exn t.non_integral_ints tvar)
;;

let undo_entry t ({ kind; _ } : Trail_entry.t) =
  match kind with
  | Add_constraint constraint_ ->
    Simplex.remove_constraint t.simplex ~constraint_
  | Set_integral { tvar; previous } ->
    (match previous with
     | true -> Hash_set.add t.integral_tvars tvar
     | false -> Hash_set.remove t.integral_tvars tvar);
    refresh_non_integral t ~tvar
;;

let rec undo t ~to_decision_level_excl =
  match Vec.Value.length t.trail with
  | 0 -> ()
  | len ->
    let trail_entry = Vec.Value.get t.trail (len - 1) in
    (match trail_entry.decision_level > to_decision_level_excl with
     | false -> t.current_decision_level <- trail_entry.decision_level
     | true ->
       ignore (Vec.Value.pop_exn t.trail : Trail_entry.t);
       undo_entry t trail_entry;
       undo t ~to_decision_level_excl)
;;

let push_trail t ~decision_level ~kind =
  Vec.Value.push t.trail ({ decision_level; kind } : Trail_entry.t)
;;

(* Called when [tvar]'s type becomes known (or is retracted) elsewhere, so that
   [non_integral_ints] -- and hence whether [solve] branches on [tvar] -- stays
   in sync with its current simplex assignment. *)
let set_integral t ~decision_level ~tvar ~integral =
  let previous = Hash_set.mem t.integral_tvars tvar in
  if Bool.( <> ) previous integral
  then (
    push_trail t ~decision_level ~kind:(Set_integral { tvar; previous });
    (match integral with
     | true -> Hash_set.add t.integral_tvars tvar
     | false -> Hash_set.remove t.integral_tvars tvar);
    refresh_non_integral t ~tvar)
;;

let simplex_var_for_tvar t ~tvar =
  Hashtbl.find_or_add t.simplex_var_by_tvar tvar ~default:(fun () ->
    let simplex_var = Simplex.add_var t.simplex in
    Hashtbl.set t.tvar_by_simplex_var ~key:simplex_var ~data:tvar;
    simplex_var)
;;

let add_constraint
  t
  ~decision_level
  ~(op : Simplex.Op.t)
  ~le:({ coeffs; const } : Linear_expr.t)
  ~c
  =
  let c = Q.(c - const) in
  let coeffs : (Q.t * int) list =
    Map.to_alist coeffs
    |> List.map ~f:(fun (tvar, q) -> q, simplex_var_for_tvar t ~tvar)
  in
  let constraint_ = Simplex.add_constraint t.simplex (coeffs, op, c) in
  push_trail t ~decision_level ~kind:(Add_constraint constraint_)
;;

let assert_atom t ~decision_level ~(atom : Atom.t) ~value =
  match value, atom with
  | true, `Type_eq (Type_expr.Var tvar, Base Int)
  | true, `Type_eq (Base Int, Type_expr.Var tvar) ->
    set_integral t ~decision_level ~tvar ~integral:true
  | true, `Type_eq (Type_expr.Var _, Base Float)
  | true, `Type_eq (Base Float, Type_expr.Var _) -> ()
  | true, `Type_eq (_, _) | false, `Type_eq _ -> ()
  | true, `Le (le, c) -> add_constraint t ~op:`Le ~le ~c ~decision_level
  | false, `Le (le, c) ->
    add_constraint
      t
      ~op:`Ge
      ~le:(Linear_expr.scale Q.(neg one) le)
      ~c:(Q.neg c)
      ~decision_level
;;

let simplex_solve t =
  let res = Simplex.solve t.simplex in
  let new_assignments = Simplex.new_assignments t.simplex in
  let rec go () =
    match Hash_queue.dequeue_front_with_key new_assignments with
    | None -> ()
    | Some (var, q) ->
      (match Hashtbl.find t.tvar_by_simplex_var var with
       | None -> ()
       | Some tvar ->
         (match Hash_set.mem t.integral_tvars tvar with
          | false -> ()
          | true ->
            let marked_nonintegral = Hash_queue.mem t.non_integral_ints tvar in
            if marked_nonintegral && Simplex.Q_eps.is_integral q
            then Hash_queue.remove_exn t.non_integral_ints tvar
            else if (not marked_nonintegral)
                    && not (Simplex.Q_eps.is_integral q)
            then Hash_queue.enqueue_front_exn t.non_integral_ints tvar ()));
      go ()
  in
  go ();
  res
;;

(* let maybe_get_lemma t = *)
(* match simplex_solve t with *)
(* | `Unsat -> Simplex.fold_conflict_row t.simplex ~f:(fun ~var_id ~bound_side
   -> *)

(* ) *)

let rec solve t =
  let[@inline always] try_with var op const =
    let constraint_ =
      Simplex.add_constraint t.simplex ([ Q.one, var ], op, const)
    in
    push_trail
      t
      ~decision_level:t.current_decision_level
      ~kind:(Add_constraint constraint_);
    let res = solve t in
    (match res with
     | `Unsat -> undo_entry t (Vec.Value.pop_exn t.trail)
     | `Sat -> ());
    res
  in
  match simplex_solve t with
  | `Unsat -> `Unsat
  | `Sat ->
    (* relaxed problem is feasible *)
    (match Hash_queue.first_with_key t.non_integral_ints with
     | None -> `Sat
     | Some (nonintegral, ()) ->
       let var = Hashtbl.find_exn t.simplex_var_by_tvar nonintegral in
       let assignment = (Simplex.assignment t.simplex ~var).value in
       (match try_with var `Le (Q.floor assignment) with
        | `Sat -> `Sat
        | `Unsat -> try_with var `Ge (Q.ceil assignment)))
;;
