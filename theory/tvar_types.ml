open! Core
open! Feel.Import
module Type = Type_expr.Base

module Atom = struct
  type t = [ `Has_type of Tvar.t * Type_expr.t ]
  [@@deriving sexp, compare, hash]

  let normalize x = x

  include functor Comparable.Make
  include functor Hashable.Make
end

type trail_entry =
  { decision_level : int
  ; var : Tvar.t
  ; old_type : Type_expr.t option
  ; caused_conflict : bool
  }

type t =
  { types : Type_expr.t Tvar.Table.t
  ; atom_by_sat_var : Atom.t Int.Table.t
  ; sat_var_by_atom : int Atom.Table.t
  ; trail : trail_entry Vec.Value.t
  ; mutable conflict : (int * int) option
  }

let create () =
  { types = Tvar.Table.create ()
  ; atom_by_sat_var = Int.Table.create ()
  ; sat_var_by_atom = Atom.Table.create ()
  ; trail = Vec.Value.create ()
  ; conflict = None
  }
;;

let add_atom t ~(atom : Atom.t) ~sat_var =
  Hashtbl.set t.atom_by_sat_var ~key:sat_var ~data:atom;
  Hashtbl.set t.sat_var_by_atom ~key:atom ~data:sat_var
;;

let sat_var_for t var type_expr =
  Hashtbl.find t.sat_var_by_atom (`Has_type (var, type_expr))
;;

let get_type t var = Hashtbl.find t.types var

(* True if [t1] and [t2] have incompatible heads — they cannot be unified. Type
   variables ([Var _]) are never incompatible since they can unify with
   anything; deeper argument mismatches are caught by type-level EUF. *)
let are_structurally_incompatible t1 t2 =
  match t1, t2 with
  | Type_expr.Base b1, Type_expr.Base b2 -> not (Type_expr.Base.equal b1 b2)
  | Type_expr.Base _, Type_expr.App _ | Type_expr.App _, Type_expr.Base _ ->
    true
  | Type_expr.App (c1, _), Type_expr.App (c2, _) -> not (Tvar.equal c1 c2)
  | Type_expr.Var _, _ | _, Type_expr.Var _ -> false
;;

let assert_literal t ~decision_level ~literal =
  match literal > 0, Hashtbl.find t.atom_by_sat_var (Int.abs literal) with
  | true, Some (`Has_type (var, type_expr)) ->
    let old_type = Hashtbl.find t.types var in
    let caused_conflict =
      match old_type with
      | Some old when not ([%compare.equal: Type_expr.t] old type_expr) ->
        if are_structurally_incompatible old type_expr
        then (
          let other_sat_var =
            Hashtbl.find_exn t.sat_var_by_atom (`Has_type (var, old))
          in
          t.conflict <- Some (Int.abs literal, other_sat_var);
          true)
        else false
      | _ -> false
    in
    Vec.Value.push t.trail { decision_level; var; old_type; caused_conflict };
    Hashtbl.set t.types ~key:var ~data:type_expr
  | _ -> ()
;;

let maybe_get_lemma t = exclave_
  match t.conflict with
  | None -> `Consistent
  | Some (sv1, sv2) -> `Lemma { Modes.Global.global = [| -sv1; -sv2 |] }
;;

let undo t ~to_decision_level_excl =
  let rec go () =
    match Vec.Value.last t.trail with
    | Some entry when entry.decision_level > to_decision_level_excl ->
      ignore (Vec.Value.pop_exn t.trail : trail_entry);
      if entry.caused_conflict then t.conflict <- None;
      (match entry.old_type with
       | None -> Hashtbl.remove t.types entry.var
       | Some type_expr -> Hashtbl.set t.types ~key:entry.var ~data:type_expr);
      go ()
    | None | Some _ -> ()
  in
  go ()
;;

let on_new_var _t ~var:_ = ()
