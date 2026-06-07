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

module Signature = struct
  type t =
    { function_ : Tvar.t
    ; args : int list
    }
  [@@deriving sexp, compare, hash]

  include functor Hashable.Make
end

module Trail_entry = struct
  module Kind = struct
    type (_ : (value & value & value) & value) tag =
      | Undo : #(Ufdsu.Undo_entry.t * Atom.t) tag
      | Falsehood : #(#(Atom.t * unit * unit) * unit) tag
      | Signature_added : #(#(Signature.t * unit * unit) * unit) tag
      | Signature_changed :
          #(#(Signature.t * old_term:Term.t * unit) * unit) tag

    type t = T : #('a tag * 'a) -> t [@@unboxed]

    let undo ~(undo_entry : Ufdsu.Undo_entry.t) ~atom =
      T #(Undo, #(undo_entry, atom))
    ;;

    let falsehood ~atom = T #(Falsehood, #(#(atom, (), ()), ()))

    let signature_changed ~signature ~old_term =
      T #(Signature_changed, #(#(signature, ~old_term, ()), ()))
    ;;

    let signature_added ~signature =
      T #(Signature_added, #(#(signature, (), ()), ()))
    ;;
  end

  type t =
    #{ kind : Kind.t
     ; decision_level : int
     }

  let fake =
    #{ kind =
         Kind.undo
           ~undo_entry:#{ child = 0; new_root = 0; rank_incremented = false }
           ~atom:(`Eq (`Var (Tvar.of_string "a"), `Var (Tvar.of_string "b")))
     ; decision_level = 0
     }
  ;;

  let create_for_vec () = fake

  include functor Vecable.Make [@kind (value & (value & value & value) & value) & value]
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
  ; mutable current_decision_level : int
  }

let rec undo t ~to_decision_level_excl =
  match Trail_entry.Vec.length t.trail with
  | 0 -> ()
  | len ->
    let trail_entry = Trail_entry.Vec.get t.trail (len - 1) in
    (match trail_entry.#decision_level > to_decision_level_excl with
     | false -> t.current_decision_level <- trail_entry.#decision_level
     | true ->
       ignore (Trail_entry.Vec.pop_exn t.trail : Trail_entry.t);
       (match trail_entry.#kind with
        | T #(Falsehood, #(atom, (), ())) -> Hash_set.remove t.falsehoods atom
        | T #(Signature_added, #(signature, (), ())) ->
          Hashtbl.remove t.signature_to_canonical signature
        | T #(Signature_changed, #(signature, ~old_term, ())) ->
          Hashtbl.set t.signature_to_canonical ~key:signature ~data:old_term
        | T #(Undo, undo_entry) -> Ufdsu.undo t.ufdsu ~undo_entry);
       undo t ~to_decision_level_excl)
;;

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
  ; current_decision_level = 0
  }
;;

let canonical_signature_term t ~term : Term.t or_null =
  match signature t ~term:(term : Term.t) with
  | Null -> Null
  | This signature ->
    This
      (Hashtbl.find_or_add
         t.signature_to_canonical
         signature
         ~default:(fun () ->
           Trail_entry.Vec.push
             t.trail
             #{ decision_level = t.current_decision_level
              ; kind = Trail_entry.Kind.signature_added ~signature
              };
           term)
      [@nontail])
;;

let assert_true_atom t ~decision_level ~(atom : Atom.t) =
  let worklist = Vec.Value.create () in
  Vec.Value.push worklist atom;
  let rec go () =
    match Vec.Value.length worklist with
    | 0 -> ()
    | _ ->
      let (`Eq (term1, term2)) = Vec.Value.pop_exn worklist in
      let t1 = canonical_ufds_var t ~term:term1 in
      let t2 = canonical_ufds_var t ~term:term2 in
      (match Ufdsu.same_class t.ufdsu t1 t2 with
       | true -> ()
       | false ->
         (* TODO: ideally calculation of [could_change] is done more efficiently *)
         let could_change term =
           Hashtbl.find_or_null t.parents term
           |> Or_null.map ~f:(fun hash_set -> Hash_set.to_list hash_set)
           |> Or_null.value ~default:[]
         in
         let could_change =
           could_change term1 @ could_change term2
           |> List.dedup_and_sort ~compare:[%compare: Term.t]
         in
         List.iter could_change ~f:(fun term ->
           (* TODO: use a signature table so we don't recompute always *)
           match signature t ~term with
           | Null -> ()
           | This signature ->
             (match
                Hashtbl.find_and_remove t.signature_to_canonical signature
              with
              | None -> ()
              | Some old_term ->
                Trail_entry.Vec.push
                  t.trail
                  #{ decision_level
                   ; kind =
                       Trail_entry.Kind.signature_changed ~signature ~old_term
                   }));
         let undo_entry =
           (Ufdsu.union t.ufdsu t1 t2 : Ufdsu.Undo_entry.Option_u.t)
           |> Ufdsu.Undo_entry.Option_u.value_exn
         in
         Trail_entry.Vec.push
           t.trail
           #{ decision_level; kind = Trail_entry.Kind.undo undo_entry };
         List.iter could_change ~f:(fun term ->
           match canonical_signature_term t ~term with
           | Null -> ()
           | This canonical ->
             (match
                Ufdsu.same_class
                  t.ufdsu
                  (canonical_ufds_var t ~term)
                  (canonical_ufds_var t ~term:canonical)
              with
              | true -> (* do nothing *) ()
              | false ->
                (* need to merge *)
                Vec.Value.push worklist (`Eq (term, canonical))));
         go ())
  in
  go ()
;;

let assert_false_atom t ~decision_level ~(atom : Atom.t) =
  match Hash_set.mem t.falsehoods atom with
  | true -> ()
  | false ->
    Hash_set.add t.falsehoods atom;
    Trail_entry.Vec.push
      t.trail
      #{ decision_level; kind = Trail_entry.Kind.falsehood atom }
;;

let assert_atom t ~decision_level ~(atom : Atom.t) ~value =
  t.current_decision_level <- decision_level;
  let atom = Atom.normalize atom in
  match value with
  | true -> assert_true_atom t ~decision_level ~atom
  | false -> assert_false_atom t ~decision_level ~atom
;;
