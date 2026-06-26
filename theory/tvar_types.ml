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
  ; trail : trail_entry Vec.Value.t
  ; mutable conflict : (Atom.t * Atom.t) option
  }

let create () =
  { types = Tvar.Table.create (); trail = Vec.Value.create (); conflict = None }
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

let assert_atom t ~decision_level ~(atom : Atom.t) ~value =
  match value, atom with
  | true, `Has_type (var, type_expr) ->
    let old_type = Hashtbl.find t.types var in
    let caused_conflict =
      match old_type with
      | Some old when not ([%compare.equal: Type_expr.t] old type_expr) ->
        if are_structurally_incompatible old type_expr
        then (
          t.conflict <- Some (atom, `Has_type (var, old));
          true)
        else false
      | _ -> false
    in
    Vec.Value.push t.trail { decision_level; var; old_type; caused_conflict };
    Hashtbl.set t.types ~key:var ~data:type_expr
  | false, `Has_type _ -> ()
;;

let maybe_get_lemma t =
  match t.conflict with
  | None -> `Consistent
  | Some (a1, a2) -> `Lemma [ a1, false; a2, false ]
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
