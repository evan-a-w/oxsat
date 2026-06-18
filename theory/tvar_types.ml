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
  }

type t =
  { types : Type.t Tvar.Table.t
  ; atom_by_sat_var : Atom.t Int.Table.t
  ; sat_var_by_atom : int Atom.Table.t
  ; trail : trail_entry Vec.Value.t
  }

let create () =
  { types = Tvar.Table.create ()
  ; atom_by_sat_var = Int.Table.create ()
  ; sat_var_by_atom = Atom.Table.create ()
  ; trail = Vec.Value.create ()
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
    Vec.Value.push t.trail { decision_level; var; old_type };
    Hashtbl.set t.types ~key:var ~data:typ
  | _ -> ()
;;

let maybe_get_lemma _t = exclave_ `Consistent

let undo t ~to_decision_level_excl =
  let rec go () =
    match Vec.Value.last t.trail with
    | Some entry when entry.decision_level > to_decision_level_excl ->
      ignore (Vec.Value.pop_exn t.trail : trail_entry);
      (match entry.old_type with
       | None -> Hashtbl.remove t.types entry.var
       | Some typ -> Hashtbl.set t.types ~key:entry.var ~data:typ);
      go ()
    | None | Some _ -> ()
  in
  go ()
;;

let on_new_var _t ~var:_ = ()
