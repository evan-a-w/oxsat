open! Core
open! Import
module Type = Type_expr.Base

module Atom = struct
  type t = [ `Type_eq of Type_expr.t * Type_expr.t ]
  [@@deriving sexp, compare, hash]

  let normalize x = x

  include functor Comparable.Make
  include functor Hashable.Make
end

let has_type var type_expr : Atom.t = `Type_eq (Type_expr.Var var, type_expr)

module Trail_entry = struct
  type t =
    { decision_level : int
    ; var : Tvar.t
    ; old_type : Type_expr.t option
    ; caused_conflict : bool
    }

  let default =
    { decision_level = 0
    ; var = Tvar.of_string ""
    ; old_type = None
    ; caused_conflict = false
    }
  ;;
end

type t =
  { types : Type_expr.t Tvar.Table.t
  ; trail : Trail_entry.t Ext.t Vec.Value.t
  ; trail_entry_pool : Trail_entry.t Ext.Pool.t
  ; mutable conflict : (Atom.t * Atom.t) option
  }

let create () =
  { types = Tvar.Table.create ()
  ; trail = Vec.Value.create ()
  ; trail_entry_pool = Ext.Pool.create_unchecked ~default:Trail_entry.default ()
  ; conflict = None
  }
;;

let get_type t var = Hashtbl.find t.types var
let all_typed_vars t = Hashtbl.keys t.types

(* True if [t1] and [t2] have incompatible heads — they cannot be unified. Type
   variables ([Var _]) are never incompatible since they can unify with
   anything; deeper argument mismatches are caught by type-level EUF. *)
let are_structurally_incompatible t1 t2 =
  match t1, t2 with
  | Type_expr.Base b1, Type_expr.Base b2 -> not (Type_expr.Base.equal b1 b2)
  | Type_expr.App (c1, _), Type_expr.App (c2, _) -> not (Tvar.equal c1 c2)
  | Type_expr.Function_type _, Type_expr.Function_type _ -> false
  | Type_expr.Type, Type_expr.Type -> false
  | Type_expr.Var _, _
  | _, Type_expr.Var _
  | Type_expr.Type_of _, _
  | _, Type_expr.Type_of _ -> false
  | Type_expr.Base _, Type_expr.App _
  | Type_expr.App _, Type_expr.Base _
  | Type_expr.Base _, Type_expr.Function_type _
  | Type_expr.Function_type _, Type_expr.Base _
  | Type_expr.Base _, Type_expr.Type
  | Type_expr.Type, Type_expr.Base _
  | Type_expr.App _, Type_expr.Function_type _
  | Type_expr.Function_type _, Type_expr.App _
  | Type_expr.App _, Type_expr.Type
  | Type_expr.Type, Type_expr.App _
  | Type_expr.Function_type _, Type_expr.Type
  | Type_expr.Type, Type_expr.Function_type _ -> true
;;

let assert_atom t ~decision_level ~(atom : Atom.t) ~value =
  match value, atom with
  | true, `Type_eq (Type_expr.Var var, type_expr)
  | true, `Type_eq (type_expr, Type_expr.Var var) ->
    let old_type = Hashtbl.find t.types var in
    let caused_conflict =
      match old_type with
      | Some old when not ([%compare.equal: Type_expr.t] old type_expr) ->
        if are_structurally_incompatible old type_expr
        then (
          t.conflict <- Some (atom, has_type var old);
          true)
        else false
      | _ -> false
    in
    Vec.Value.push
      t.trail
      (Ext.alloc_set
         t.trail_entry_pool
         (stack_ { decision_level; var; old_type; caused_conflict }
          : Trail_entry.t));
    Hashtbl.set t.types ~key:var ~data:type_expr
  | true, `Type_eq (_, _) | false, `Type_eq _ -> ()
;;

let maybe_get_lemma t =
  match t.conflict with
  | None -> `Consistent
  | Some (a1, a2) -> `Lemma [ a1, false; a2, false ]
;;

let undo t ~to_decision_level_excl =
  let rec go () =
    match Vec.Value.last t.trail with
    | Some entry_ext
      when (Ext.get entry_ext).decision_level > to_decision_level_excl ->
      let entry_ext = Vec.Value.pop_exn t.trail in
      let entry = Ext.get entry_ext in
      if entry.caused_conflict then t.conflict <- None;
      (match entry.old_type with
       | None -> Hashtbl.remove t.types entry.var
       | Some type_expr -> Hashtbl.set t.types ~key:entry.var ~data:type_expr);
      Ext.free entry_ext;
      go ()
    | None | Some _ -> ()
  in
  go ()
;;
