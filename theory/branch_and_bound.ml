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
      | Add_constraint of Simplex.Constraint.t
      | Set_integral of
          { tvar : Tvar.t
          ; previous : (Atom.t * bool) option
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
  ; (* The [`Type_eq] atom (and its value) that most recently made a tvar
       integral, so a case-split lemma for that tvar can be guarded on it. *)
    integral_atom_by_tvar : (Atom.t * bool) Tvar.Table.t
  ; non_integral_ints : unit Tvar.Hash_queue.t
  ; simplex : Simplex.t
  ; trail : Trail_entry.t Vec.Value.t
  ; atom_for_constraint : (Atom.t * bool) Simplex.Constraint.Table.t
  }

let create () : t =
  { simplex_var_by_tvar = Tvar.Table.create ()
  ; tvar_by_simplex_var = Int.Table.create ()
  ; integral_tvars = Tvar.Hash_set.create ()
  ; integral_atom_by_tvar = Tvar.Table.create ()
  ; non_integral_ints = Tvar.Hash_queue.create ()
  ; simplex = Simplex.create ()
  ; trail = Vec.Value.create ()
  ; atom_for_constraint = Simplex.Constraint.Table.create ()
  }
;;

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
    Simplex.remove_constraint t.simplex ~constraint_;
    Hashtbl.remove t.atom_for_constraint constraint_
  | Set_integral { tvar; previous } ->
    (match previous with
     | Some atom_and_value ->
       Hash_set.add t.integral_tvars tvar;
       Hashtbl.set t.integral_atom_by_tvar ~key:tvar ~data:atom_and_value
     | None ->
       Hash_set.remove t.integral_tvars tvar;
       Hashtbl.remove t.integral_atom_by_tvar tvar);
    refresh_non_integral t ~tvar
;;

let rec undo t ~to_decision_level_excl =
  match Vec.Value.length t.trail with
  | 0 -> ()
  | len ->
    let trail_entry = Vec.Value.get t.trail (len - 1) in
    (match trail_entry.decision_level > to_decision_level_excl with
     | false -> ()
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
let set_integral t ~decision_level ~tvar ~integral ~(atom : Atom.t) ~value =
  let was_integral = Hash_set.mem t.integral_tvars tvar in
  if Bool.( <> ) was_integral integral
  then (
    let previous = Hashtbl.find t.integral_atom_by_tvar tvar in
    push_trail t ~decision_level ~kind:(Set_integral { tvar; previous });
    (match integral with
     | true ->
       Hash_set.add t.integral_tvars tvar;
       Hashtbl.set t.integral_atom_by_tvar ~key:tvar ~data:(atom, value)
     | false ->
       Hash_set.remove t.integral_tvars tvar;
       Hashtbl.remove t.integral_atom_by_tvar tvar);
    refresh_non_integral t ~tvar)
;;

let simplex_var_for_tvar t ~tvar =
  Hashtbl.find_or_add t.simplex_var_by_tvar tvar ~default:(fun () ->
    let simplex_var = Simplex.add_var t.simplex in
    Hashtbl.set t.tvar_by_simplex_var ~key:simplex_var ~data:tvar;
    simplex_var)
;;

let rec update_state_for_new_assignments t =
  let new_assignments = Simplex.new_assignments t.simplex in
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
          else if (not marked_nonintegral) && not (Simplex.Q_eps.is_integral q)
          then Hash_queue.enqueue_front_exn t.non_integral_ints tvar ()));
    update_state_for_new_assignments t
;;

let add_constraint
  t
  ~decision_level
  ~(op : Simplex.Op.t)
  ~le:({ coeffs; const } : Linear_expr.t)
  ~c
  ~atom
  ~value
  =
  let c = Q.(c - const) in
  let coeffs : (Q.t * int) list =
    Map.to_alist coeffs
    |> List.map ~f:(fun (tvar, q) -> q, simplex_var_for_tvar t ~tvar)
  in
  let constraint_ = Simplex.add_constraint t.simplex (coeffs, op, c) in
  Hashtbl.set t.atom_for_constraint ~key:constraint_ ~data:(atom, value);
  push_trail t ~decision_level ~kind:(Add_constraint constraint_);
  update_state_for_new_assignments t
;;

let assert_atom t ~decision_level ~(atom : Atom.t) ~value =
  match value, atom with
  | true, `Type_eq (Type_expr.Var tvar, Base Int)
  | true, `Type_eq (Base Int, Type_expr.Var tvar) ->
    set_integral t ~decision_level ~tvar ~integral:true ~atom ~value
  | true, `Type_eq (Type_expr.Var _, Base Float)
  | true, `Type_eq (Base Float, Type_expr.Var _) -> ()
  | true, `Type_eq (_, _) | false, `Type_eq _ -> ()
  | true, `Le (le, c) ->
    add_constraint t ~op:`Le ~le ~c ~decision_level ~atom ~value
  | false, `Le (le, c) ->
    (* [Not (`Le (le, c))] is [le > c]. *)
    add_constraint t ~op:`Gt ~le ~c ~decision_level ~atom ~value
;;

let simplex_solve t =
  let res = Simplex.solve t.simplex in
  update_state_for_new_assignments t;
  res
;;

let maybe_get_lemma t =
  match simplex_solve t with
  | `Unsat conflicting_constraints ->
    `Lemma
      (List.map conflicting_constraints ~f:(fun constraint_ ->
         let atom, value = Hashtbl.find_exn t.atom_for_constraint constraint_ in
         atom, not value))
  | `Sat ->
    (match Hash_queue.first_with_key t.non_integral_ints with
     | None -> (* TODO: infer equalities *) `Consistent
     | Some (tvar, ()) ->
       (* either <= floor value or >= ceil value, but only while [tvar] is
          actually integral -- guard on the negation of the atom that asserted
          that, so the clause doesn't outlive it being retracted. *)
       let integral_atom, integral_value =
         Hashtbl.find_exn t.integral_atom_by_tvar tvar
       in
       let assignment =
         (Simplex.assignment t.simplex ~var:(simplex_var_for_tvar t ~tvar))
           .value
       in
       (* this is NOT a unit clause, and doesn't depend on the current
          assignments / constraints etc. However, this is always true for
          integers. We do need to depend on the type of the var being an
          integer, because it's not always true for floats *)
       `Lemma
         [ integral_atom, not integral_value
         ; ( `Le
               ( ({ const = Q.zero
                  ; coeffs = Tvar.Map.of_alist_exn [ tvar, Q.one ]
                  }
                  : Linear_expr.t)
               , Q.floor assignment )
           , true )
         ; ( `Le
               ( Linear_expr.neg
                   ({ const = Q.zero
                    ; coeffs = Tvar.Map.of_alist_exn [ tvar, Q.one ]
                    }
                    : Linear_expr.t)
               , Q.(ceil assignment |> neg) )
           , true )
         ])
;;

let assignment t ~tvar =
  Hashtbl.find t.simplex_var_by_tvar tvar
  |> Option.map ~f:(fun var -> Simplex.assignment t.simplex ~var)
;;

let all_numeric_vars t = Hashtbl.keys t.simplex_var_by_tvar
