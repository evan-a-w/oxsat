open! Core
open! Feel.Import

module Type = struct
  type t =
    | Int
    | Float
  [@@deriving sexp, compare, equal, hash]
end

module Atom = struct
  type t = [ `Has_type of Tvar.t * Type.t ] [@@deriving sexp, compare, hash]

  let normalize x = x

  include functor Comparable.Make
  include functor Hashable.Make
end

type trail_entry =
  { decision_level : int
  ; var : Tvar.t
  ; old_type : Type.t option
  ; caused_conflict : bool
  }

type t =
  { types : Type.t Tvar.Table.t
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

let sat_var_for t var typ =
  Hashtbl.find t.sat_var_by_atom (`Has_type (var, typ))
;;

let get_type t var = Hashtbl.find t.types var

let assert_literal t ~decision_level ~literal =
  match literal > 0, Hashtbl.find t.atom_by_sat_var (Int.abs literal) with
  | true, Some (`Has_type (var, typ)) ->
    let old_type = Hashtbl.find t.types var in
    let caused_conflict =
      match old_type with
      | Some old when not (Type.equal old typ) ->
        let other_sat_var =
          Hashtbl.find_exn t.sat_var_by_atom (`Has_type (var, old))
        in
        t.conflict <- Some (Int.abs literal, other_sat_var);
        true
      | _ -> false
    in
    Vec.Value.push t.trail { decision_level; var; old_type; caused_conflict };
    Hashtbl.set t.types ~key:var ~data:typ
  | _ -> ()
;;

let maybe_get_lemma t = exclave_
  match t.conflict with
  | None -> `Consistent
  | Some (sv1, sv2) ->
    `Lemma { Modes.Global.global = [| -sv1; -sv2 |] }
;;

let undo t ~to_decision_level_excl =
  let rec go () =
    match Vec.Value.last t.trail with
    | Some entry when entry.decision_level > to_decision_level_excl ->
      ignore (Vec.Value.pop_exn t.trail : trail_entry);
      if entry.caused_conflict then t.conflict <- None;
      (match entry.old_type with
       | None -> Hashtbl.remove t.types entry.var
       | Some typ -> Hashtbl.set t.types ~key:entry.var ~data:typ);
      go ()
    | None | Some _ -> ()
  in
  go ()
;;

let on_new_var _t ~var:_ = ()
