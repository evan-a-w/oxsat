open! Core
open! Feel.Import

type t =
  | True
  | False
  | Atom of Atom.t
  | Not of t
  | And of t list
  | Or of t list
[@@deriving sexp]

let rec fold_map_atoms t ~init ~(f : _ @ local) =
  match t with
  | True | False -> init, t
  | Not t ->
    let a, t = fold_map_atoms t ~init ~f in
    a, Not t
  | And ts ->
    let a, ts =
      List.fold_map ts ~init ~f:(fun acc x -> fold_map_atoms x ~init:acc ~f)
    in
    a, And ts
  | Or ts ->
    let a, ts =
      List.fold_map ts ~init ~f:(fun acc x -> fold_map_atoms x ~init:acc ~f)
    in
    a, Or ts
  | Atom a ->
    let a, atom = f init a in
    a, Atom atom
;;

module Encoding = struct
  type t =
    { mutable next_var : int
    ; atom_to_sat_var : int Atom.Table.t
    ; sat_var_to_atom : Atom.t Int.Table.t
    ; atoms_in_order : (Atom.t * int) Vec.Value.t
    ; mutable true_var : int or_null
    }

  let create () =
    { next_var = 1
    ; atom_to_sat_var = Atom.Table.create ()
    ; sat_var_to_atom = Int.Table.create ()
    ; atoms_in_order = Vec.Value.create ()
    ; true_var = Null
    }
  ;;

  let fresh_var t =
    let var = t.next_var in
    t.next_var <- t.next_var + 1;
    var
  ;;

  let sat_var_for_atom t atom =
    let atom = Atom.normalize atom in
    Hashtbl.find_or_add t.atom_to_sat_var atom ~default:(fun () ->
      let sat_var = fresh_var t in
      Hashtbl.set t.sat_var_to_atom ~key:sat_var ~data:atom;
      Vec.Value.push t.atoms_in_order (atom, sat_var);
      sat_var)
  ;;

  let find_sat_var_for_atom t atom =
    Hashtbl.find t.atom_to_sat_var (Atom.normalize atom)
  ;;

  let atom_for_sat_var t sat_var = Hashtbl.find t.sat_var_to_atom sat_var
  let atoms t = Vec.Value.to_list t.atoms_in_order
  let checkpoint t = Vec.Value.length t.atoms_in_order

  let new_atoms_since t ~checkpoint:from =
    List.init
      (Vec.Value.length t.atoms_in_order - from)
      ~f:(fun i -> Vec.Value.get t.atoms_in_order (from + i))
  ;;

  (* A variable that is always true: a fresh variable with a unit clause
     asserting it, allocated lazily and cached so [True]/[False] don't waste a
     variable when unused. *)
  let true_literal t ~clauses =
    match t.true_var with
    | This var -> var
    | Null ->
      let var = fresh_var t in
      Vec.Value.push clauses [| var |];
      t.true_var <- This var;
      var
  ;;
end

(* Returns a literal representing [formula]'s truth value, pushing any Tseitin
   definition clauses for subformulas onto [clauses]. *)
let rec literal_of (encoding : Encoding.t) ~clauses (formula : t) : int =
  match formula with
  | True -> Encoding.true_literal encoding ~clauses
  | False -> -Encoding.true_literal encoding ~clauses
  | Atom atom -> Encoding.sat_var_for_atom encoding atom
  | Not f -> -literal_of encoding ~clauses f
  | And fs ->
    let literals = List.map fs ~f:(literal_of encoding ~clauses) in
    let d = Encoding.fresh_var encoding in
    (* d -> l_i, for each i *)
    List.iter literals ~f:(fun l -> Vec.Value.push clauses [| -d; l |]);
    (* (l_1 /\ ... /\ l_n) -> d *)
    Vec.Value.push clauses (Array.of_list (d :: List.map literals ~f:( ~- )));
    d
  | Or fs ->
    let literals = List.map fs ~f:(literal_of encoding ~clauses) in
    let d = Encoding.fresh_var encoding in
    (* l_i -> d, for each i *)
    List.iter literals ~f:(fun l -> Vec.Value.push clauses [| -l; d |]);
    (* d -> (l_1 \/ ... \/ l_n) *)
    Vec.Value.push clauses (Array.of_list (-d :: literals));
    d
;;

let encode (encoding : Encoding.t) (formula : t) : int array list =
  let clauses = Vec.Value.create () in
  let root = literal_of encoding ~clauses formula in
  Vec.Value.push clauses [| root |];
  Vec.Value.to_list clauses
;;
