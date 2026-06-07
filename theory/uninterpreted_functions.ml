open! Core
open! Feel.Import

module Term = struct
  type t =
    [ `App of function_:Tvar.t * args:t list
    | `Var of Tvar.t
    ]
  [@@deriving sexp, compare, hash]

  include functor Comparable.Make
  include functor Hashable.Make
end

module Atom = struct
  type t = [ `Eq of Term.t * Term.t ] [@@deriving sexp, compare, hash]

  let normalize = function
    | `Eq (a, b) as x ->
      (match Ordering.of_int ([%compare: Term.t] a b) with
       | Equal | Less -> x
       | Greater -> `Eq (b, a))
  ;;

  include functor Comparable.Make
  include functor Hashable.Make
end

module Atom_data = struct
  type t =
    { atom : Atom.t
    ; sat_var : int
    ; mutable assignment : bool or_null [@compare.ignore]
    }
  [@@deriving hash, compare, sexp]
end

module Trail_entry = struct
  module Kind = struct
    type (_ : value & value & value, _) tag =
      | Undo : (Ufdsu.Undo_entry.t, _) tag
      | Falsehood : (_, Atom.t) tag

    type t = T : #(('a, 'b) tag * 'a * 'b) -> t [@@unboxed]

    let undo (undo_entry : Ufdsu.Undo_entry.t) = T #(Undo, undo_entry, ())
    let falsehood atom = T #(Falsehood, #((), (), ()), atom)
  end

  type t =
    #{ kind : Kind.t
     ; decision_level : int
     }

  let create_for_vec () =
    #{ kind = Kind.undo #{ child = 0; new_root = 0; rank_incremented = false }
     ; decision_level = 0
     }
  ;;

  include
    functor
    Vecable.Make
    [@kind (value & (value & value & value) & value) & value]
end

module Signature = struct
  type t =
    { function_ : Tvar.t
    ; args : int list
    }
  [@@deriving sexp, compare, hash]

  include functor Hashable.Make
end

type t =
  { ufds_var_by_term : int Term.Table.t
  ; ufdsu : Ufdsu.t
  ; atoms : Atom_data.t Atom.Table.t
  ; signature_to_canonical : Term.t Signature.Table.t
  ; parents : Term.Hash_set.t Term.Table.t
  ; trail : Trail_entry.Vec.t
  ; mutable trail_entry_processed_till : int
  ; falsehoods : Atom.Hash_set.t
  }

let canonical_ufds_var t ~term =
  Hashtbl.find_exn t.ufds_var_by_term term |> Ufdsu.find t.ufdsu _
;;

let signature t ~(term : Term.t) : Signature.t or_null =
  match term with
  | `Var _ -> Null
  | `App (~function_, ~args) ->
    let args = List.map args ~f:(fun term -> canonical_ufds_var t ~term) in
    This { function_; args }
;;

let create ~atoms =
  let ufds_var_by_term = Term.Table.create () in
  let signature_to_canonical = Signature.Table.create () in
  let parents = Term.Table.create () in
  let ufdsu = Ufdsu.create () in
  let on_term term =
    match Hashtbl.find ufds_var_by_term term with
    | Some _ -> ()
    | None ->
      let ufds_var = Ufdsu.add ufdsu in
      Hashtbl.set ufds_var_by_term ~key:term ~data:ufds_var;
      (match (term : Term.t) with
       | `Var _ -> ()
       | `App (~function_:_, ~args) ->
         List.iter args ~f:(fun arg ->
           let parents =
             Hashtbl.find_or_add parents arg ~default:Term.Hash_set.create
           in
           Hash_set.add parents term))
  in
  let atoms =
    List.map atoms ~f:(fun (atom, sat_var) : (Atom.t * Atom_data.t) ->
      let atom = Atom.normalize atom in
      (match atom with
       | `Eq (a, b) ->
         on_term a;
         on_term b);
      atom, { atom; sat_var; assignment = Null })
    |> Atom.Table.of_alist_exn
  in
  { ufds_var_by_term
  ; ufdsu
  ; atoms
  ; parents
  ; trail = Trail_entry.Vec.create ()
  ; trail_entry_processed_till = 0
  ; signature_to_canonical
  ; falsehoods = Atom.Hash_set.create ()
  }
;;

(* let assert_atom t ~decision_level ~(atom : Atom.t) ~value = *)
(* let atom = Atom.normalize atom in *)
(* match atom, value with *)
(* | `Eq (term1, term2), true -> () *)
(* ;; *)
