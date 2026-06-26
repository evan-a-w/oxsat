open! Core
open! Feel.Import

type t =
  { sat_var_by_atom : int Uninterpreted_functions.Atom.Table.t
  ; atom_by_sat_var : Uninterpreted_functions.Atom.t Int.Table.t
  }

let create () =
  { sat_var_by_atom = Uninterpreted_functions.Atom.Table.create ()
  ; atom_by_sat_var = Int.Table.create ()
  }
;;

let add t ~(atom : Uninterpreted_functions.Atom.t) ~sat_var =
  let atom = Uninterpreted_functions.Atom.normalize atom in
  Hashtbl.set t.sat_var_by_atom ~key:atom ~data:sat_var;
  Hashtbl.set t.atom_by_sat_var ~key:sat_var ~data:atom
;;

let atom_for_sat_var t sat_var = Hashtbl.find t.atom_by_sat_var sat_var

let sat_var_for_atom t (atom : Uninterpreted_functions.Atom.t) =
  Hashtbl.find_exn t.sat_var_by_atom atom
;;
