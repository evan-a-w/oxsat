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

(* Tracks which sat var set each bound on a simplex variable so we can name the
   right literals in conflict lemmas. *)
module Bound_witness = struct
  type t =
    { mutable le : int option
    ; mutable ge : int option
    }

  let create () = { le = None; ge = None }
end

module Trail_entry = struct
  module Kind = struct
    type t =
      | Add_constraint of
          { constraint_ : Simplex.constraint_
          ; simplex_var : int
          ; bound_side : [ `Le | `Ge ]
          ; prev_witness : int option
          }
      | Untracked_constraint of { constraint_ : Simplex.constraint_ }
  end

  type t =
    { decision_level : int
    ; kind : Kind.t
    }
end

type t =
  { simplex_var_by_tvar_and_type : int Tvar_and_type.Table.t
  ; tvar_type_by_simplex_var : (Tvar.t * Type_expr.Base.t) Int.Table.t
  ; simplex : Simplex.t
  ; trail : Trail_entry.t Vec.Value.t
  ; mutable current_decision_level : int
  ; atom_by_sat_var : (Linear_expr.t * Q.t * Type_expr.Base.t) Int.Table.t
  ; sat_var_by_atom : int Atom.Table.t
  ; bound_witness_by_simplex_var : Bound_witness.t Int.Table.t
  ; mutable conflict : int array option
  ; sat_var_for_atom : Atom.t -> int
  }

let get_or_add_simplex_var t tvar type_ =
  Hashtbl.find_or_add
    t.simplex_var_by_tvar_and_type
    { tvar; type_ }
    ~default:(fun () ->
      let var = Simplex.add_var t.simplex in
      Hashtbl.set t.tvar_type_by_simplex_var ~key:var ~data:(tvar, type_);
      Hashtbl.set
        t.bound_witness_by_simplex_var
        ~key:var
        ~data:(Bound_witness.create ());
      var)
;;

let create ~sat_var_for_atom =
  { simplex_var_by_tvar_and_type = Tvar_and_type.Table.create ()
  ; tvar_type_by_simplex_var = Int.Table.create ()
  ; simplex = Simplex.create ()
  ; trail = Vec.Value.create ()
  ; current_decision_level = 0
  ; atom_by_sat_var = Int.Table.create ()
  ; sat_var_by_atom = Atom.Table.create ()
  ; bound_witness_by_simplex_var = Int.Table.create ()
  ; conflict = None
  ; sat_var_for_atom
  }
;;

let register_tvar_with_type t tvar type_ =
  ignore (get_or_add_simplex_var t tvar type_ : int)
;;

let add_atom t ~(atom : Atom.t) ~sat_var =
  match atom with
  | `Le (expr, c) ->
    (* Infer the type from already-registered tvars in the expression,
       defaulting to Float. Le atoms on Int variables must have their tvars
       registered first via [register_tvar_with_type]. *)
    let type_ =
      Map.fold
        expr.coeffs
        ~init:Type_expr.Base.Float
        ~f:(fun ~key:tvar ~data:_ acc ->
          match acc with
          | Int -> Int
          | Float ->
            (match
               Hashtbl.find t.simplex_var_by_tvar_and_type { tvar; type_ = Int }
             with
             | Some _ -> Int
             | None -> Float))
    in
    Hashtbl.set t.atom_by_sat_var ~key:sat_var ~data:(expr, c, type_);
    Hashtbl.set t.sat_var_by_atom ~key:atom ~data:sat_var
  | _ -> ()
;;

let push_trail t ~kind =
  Vec.Value.push
    t.trail
    ({ decision_level = t.current_decision_level; kind } : Trail_entry.t)
;;

let linear_expr_to_simplex_lhs t (expr : Linear_expr.t) type_ =
  Map.fold expr.coeffs ~init:[] ~f:(fun ~key:tvar ~data:coeff acc ->
    let var = get_or_add_simplex_var t tvar type_ in
    (coeff, var) :: acc)
;;

(* Collect the negated sat vars witnessing infeasibility of the current failing
   simplex row. For the basic var: the sat var that set its violated bound. For
   each nonbasic var with nonzero coefficient: the sat var that set the bound
   blocking the pivot. Returns [None] if no row is currently infeasible. *)
let collect_conflict_lemma t =
  let witnesses = ref [] in
  let result =
    Simplex.fold_conflict_row t.simplex ~f:(fun ~var_id ~bound_side ->
      match Hashtbl.find t.bound_witness_by_simplex_var var_id with
      | None -> ()
      | Some witness ->
        let sv =
          match bound_side with
          | `Le -> witness.le
          | `Ge -> witness.ge
        in
        Option.iter sv ~f:(fun sv -> witnesses := sv :: !witnesses))
  in
  match result with
  | None -> None
  | Some () -> Some (Array.of_list (List.map !witnesses ~f:(fun sv -> -sv)))
;;

let undo_entry t ~(trail_entry : Trail_entry.t) =
  match trail_entry.kind with
  | Add_constraint { constraint_; simplex_var; bound_side; prev_witness } ->
    Simplex.remove_constraint t.simplex ~constraint_;
    let witness = Hashtbl.find_exn t.bound_witness_by_simplex_var simplex_var in
    (match bound_side with
     | `Le -> witness.le <- prev_witness
     | `Ge -> witness.ge <- prev_witness)
  | Untracked_constraint { constraint_ } ->
    Simplex.remove_constraint t.simplex ~constraint_
;;

let rec undo t ~to_decision_level_excl =
  match Vec.Value.length t.trail with
  | 0 -> ()
  | len ->
    let trail_entry = Vec.Value.get t.trail (len - 1) in
    (match trail_entry.decision_level > to_decision_level_excl with
     | false ->
       t.current_decision_level <- trail_entry.decision_level;
       t.conflict <- None
     | true ->
       ignore (Vec.Value.pop_exn t.trail : Trail_entry.t);
       undo_entry t ~trail_entry;
       undo t ~to_decision_level_excl)
;;

let add_tracked_constraint t ~sat_var ~simplex_var ~lhs ~op ~rhs ~bound_side =
  let constraint_ = Simplex.add_constraint t.simplex (lhs, op, rhs) in
  let witness = Hashtbl.find_exn t.bound_witness_by_simplex_var simplex_var in
  let prev_witness =
    match bound_side with
    | `Le -> witness.le
    | `Ge -> witness.ge
  in
  (match bound_side with
   | `Le -> witness.le <- Some sat_var
   | `Ge -> witness.ge <- Some sat_var);
  push_trail
    t
    ~kind:
      (Add_constraint { constraint_; simplex_var; bound_side; prev_witness })
;;

let add_untracked_constraint t ~lhs ~op ~rhs =
  let constraint_ = Simplex.add_constraint t.simplex (lhs, op, rhs) in
  push_trail t ~kind:(Untracked_constraint { constraint_ })
;;

let assert_literal t ~decision_level ~literal =
  t.current_decision_level <- decision_level;
  match Hashtbl.find t.atom_by_sat_var (Int.abs literal) with
  | None -> ()
  | Some (expr, c, type_) ->
    (match t.conflict with
     | Some _ -> ()
     | None ->
       let lhs = linear_expr_to_simplex_lhs t expr type_ in
       (* For a single-var atom [x <= c] (coeff 1 on one tvar, no const), we
          attribute the bound to that simplex var for conflict witness tracking.
          Multi-var expressions are added untracked since there is no single var
          whose bound they set. *)
       let single_simplex_var =
         match Map.to_alist expr.coeffs with
         | [ (tvar, coeff) ] when Q.equal coeff Q.one && Q.is_zero expr.const ->
           Hashtbl.find t.simplex_var_by_tvar_and_type { tvar; type_ }
         | _ -> None
       in
       (match literal > 0 with
        | true ->
          let rhs = Q.(c - expr.const) in
          (match single_simplex_var with
           | Some sv ->
             add_tracked_constraint
               t
               ~sat_var:(Int.abs literal)
               ~simplex_var:sv
               ~lhs
               ~op:`Le
               ~rhs
               ~bound_side:`Le
           | None -> add_untracked_constraint t ~lhs ~op:`Le ~rhs)
        | false ->
          (* ¬(expr <= c) means expr > c *)
          let neg_lhs =
            List.map lhs ~f:(fun (coeff, var) -> Q.neg coeff, var)
          in
          (match type_ with
           | Int ->
             (* For integers, expr > c iff expr >= c + 1, i.e. -expr <= -(c+1) *)
             let rhs = Q.(neg (c + one) - expr.const) in
             (match single_simplex_var with
              | Some sv ->
                add_tracked_constraint
                  t
                  ~sat_var:(Int.abs literal)
                  ~simplex_var:sv
                  ~lhs:neg_lhs
                  ~op:`Le
                  ~rhs
                  ~bound_side:`Ge
              | None -> add_untracked_constraint t ~lhs:neg_lhs ~op:`Le ~rhs)
           | Float ->
             (* Simplex is over rationals; expr > c (strict) cannot be expressed
                as a non-strict inequality over the same rational bound. We omit
                the constraint: sound (may miss conflicts) but not complete for
                strict float inequalities. *)
             ()));
       (match Simplex.solve t.simplex with
        | `Sat -> ()
        | `Unsat -> t.conflict <- collect_conflict_lemma t))
;;

(* Scan all Int-typed simplex vars for any with a non-integral current
   assignment. Returns the first such (tvar, simplex_var, assignment) found. *)
let find_nonintegral_int t =
  Hashtbl.fold
    t.tvar_type_by_simplex_var
    ~init:None
    ~f:(fun ~key:simplex_var ~data:(tvar, type_) acc ->
      match acc, type_ with
      | Some _, _ | _, Float -> acc
      | None, Int ->
        let q = Simplex.assignment t.simplex ~var:simplex_var in
        if Q.is_integral q then None else Some (tvar, simplex_var, q))
;;

let maybe_get_lemma t = exclave_
  match t.conflict with
  | Some clause -> `Lemma { Modes.Global.global = clause }
  | None ->
    (match collect_conflict_lemma t with
     | Some clause -> `Lemma { Modes.Global.global = clause }
     | None ->
       (match find_nonintegral_int t with
        | None -> `Consistent
        | Some (nonintegral, _simplex_var, q) ->
          let floor_q = Q.floor q in
          let ceil_q = Q.ceil q in
          let tvar_expr = Linear_expr.var nonintegral in
          let le_atom : Atom.t = Atom.normalize (`Le (tvar_expr, floor_q)) in
          let ge_atom : Atom.t =
            Atom.normalize (`Le (Linear_expr.neg tvar_expr, Q.neg ceil_q))
          in
          (* Ensure both branch atoms have sat vars (allocating fresh ones via
             [sat_var_for_atom] if either hasn't been asserted before), then
             emit the tautology [x <= floor(q)] \/ [x >= ceil(q)] so the SAT
             solver -- not this theory -- decides which branch to explore and
             how to backtrack out of it. *)
          let le_sv = t.sat_var_for_atom le_atom in
          let ge_sv = t.sat_var_for_atom ge_atom in
          `Lemma { Modes.Global.global = [| le_sv; ge_sv |] }))
;;

let on_new_var _t ~var:_ = ()
