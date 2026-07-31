open! Core
open! Import

module Formula_list = struct
  module T = struct
    type t = Formula.any list [@@deriving sexp, compare, hash]
  end

  include T
  include functor Comparable.Make
  include functor Hashable.Make
end

type t =
  { array_terms : Formula.Any.Hash_set.t
  ; mutable has_declared_array_type : bool
  ; atoms : Atom.Equality.Hash_set.t
  ; row1_emitted : Formula.Any.Hash_set.t
  ; row2_emitted : Formula.Any.Hash_set.t
  ; ext_emitted : Formula_list.Hash_set.t
  ; mutable next_witness : int
  ; mutable last_certificate : Lemma_certificate.Array.t option
  }

let create () =
  { array_terms = Formula.Any.Hash_set.create ()
  ; has_declared_array_type = false
  ; atoms = Atom.Equality.Hash_set.create ()
  ; row1_emitted = Formula.Any.Hash_set.create ()
  ; row2_emitted = Formula.Any.Hash_set.create ()
  ; ext_emitted = Formula_list.Hash_set.create ()
  ; next_witness = 0
  ; last_certificate = None
  }
;;

let rec note_array_shapes t (term : Formula.any) =
  match term with
  | Select (array, index) ->
    Hash_set.add t.array_terms array;
    note_array_shapes t array;
    note_array_shapes t index
  | Store (array, index, value) ->
    Hash_set.add t.array_terms term;
    Hash_set.add t.array_terms array;
    note_array_shapes t array;
    note_array_shapes t index;
    note_array_shapes t value
  | Array_type _ ->
    t.has_declared_array_type <- true;
    List.iter (Formula.args term) ~f:(note_array_shapes t)
  | _ -> List.iter (Formula.args term) ~f:(note_array_shapes t)
;;

let add_atom t ~atom =
  let atom = Atom.Equality.normalize atom in
  Hash_set.add t.atoms atom;
  let left, right = Atom.Equality.endpoints atom in
  note_array_shapes t left;
  note_array_shapes t right
;;

let is_syntactic_array_term t = function
  | Formula.Store _ -> true
  | term -> Hash_set.mem t.array_terms term
;;

let has_declared_array_type ~get_type = function
  | Formula.Var v ->
    (match get_type v with
     | Some (Type_expr.Array_type _) -> true
     | Some
         ( Type_expr.Var _
         | Type_expr.Base _
         | Type_expr.Type_of _
         | Type_expr.App _
         | Type_expr.Function_type _
         | Type_expr.Type )
     | None -> false)
  | _ -> false
;;

module Class_info = struct
  type t =
    { mutable known_array : bool
    ; mutable syntactic_array : bool
    }

  let create () = { known_array = false; syntactic_array = false }
end

let array_class_info t egraph ~get_type =
  let info_by_repr = Formula.Any.Table.create () in
  List.iter (Formula_egraph_uf.registered_terms egraph) ~f:(fun term ->
    let repr = Formula_egraph_uf.canonical_term egraph ~term in
    let info =
      Hashtbl.find_or_add info_by_repr repr ~default:Class_info.create
    in
    if is_syntactic_array_term t term
    then (
      info.known_array <- true;
      info.syntactic_array <- true);
    if has_declared_array_type ~get_type term then info.known_array <- true);
  info_by_repr
;;

let class_info info_by_repr egraph term =
  let repr = Formula_egraph_uf.canonical_term egraph ~term in
  repr, Hashtbl.find info_by_repr repr
;;

let eq left right : Atom.Equality.t = `Eq (left, right)

let register_lemma_atoms egraph literals =
  List.iter literals ~f:(fun (atom, _) ->
    if Option.is_none (Formula_egraph_uf.atom_value egraph ~atom)
    then Formula_egraph_uf.add_atom egraph ~atom)
;;

let row1 t egraph terms =
  List.find_map terms ~f:(function
    | Formula.Store (array, index, value) as store
      when not (Hash_set.mem t.row1_emitted store) ->
      Hash_set.add t.row1_emitted store;
      let select = Formula.Select (store, index) in
      let literals = [ eq select value, true ] in
      register_lemma_atoms egraph literals;
      t.last_certificate
      <- Some (Read_over_write_same_index { array; index; value });
      Some (`Lemma literals)
    | _ -> None)
;;

let row2 t egraph terms =
  List.find_map terms ~f:(function
    | Formula.Select
        ( (Formula.Store (array, written_index, written_value) as store)
        , read_index ) as select
      when (not (Hash_set.mem t.row2_emitted select))
           && not (Formula.equal_any written_index read_index) ->
      Hash_set.add t.row2_emitted select;
      let literals =
        [ eq written_index read_index, true
        ; eq select (Formula.Select (array, read_index)), true
        ]
      in
      ignore (store : Formula.any);
      register_lemma_atoms egraph literals;
      t.last_certificate
      <- Some
           (Read_over_write_different_index
              { array; written_index; written_value; read_index });
      Some (`Lemma literals)
    | _ -> None)
;;

let normalized_pair left right =
  if Formula.compare_any left right <= 0
  then [ left; right ]
  else [ right; left ]
;;

let fresh_witness t =
  let witness =
    Formula.Var
      (Tvar.of_string (sprintf "__array_extensionality_%d" t.next_witness))
  in
  t.next_witness <- t.next_witness + 1;
  witness
;;

let extensionality t egraph ~get_type =
  let info_by_repr = array_class_info t egraph ~get_type in
  Hash_set.find_map t.atoms ~f:(function
    | `Type_eq _ -> None
    | `Eq (left, right) as atom ->
      let left_repr, left_info = class_info info_by_repr egraph left in
      let right_repr, right_info = class_info info_by_repr egraph right in
      let key = normalized_pair left_repr right_repr in
      let both_known_arrays =
        Option.exists left_info ~f:(fun info -> info.known_array)
        && Option.exists right_info ~f:(fun info -> info.known_array)
      in
      let relevant =
        Option.exists left_info ~f:(fun info -> info.syntactic_array)
        || Option.exists right_info ~f:(fun info -> info.syntactic_array)
      in
      if Hash_set.mem t.ext_emitted key
         || Formula.equal_any left_repr right_repr
         || not (both_known_arrays && relevant)
      then None
      else (
        match Formula_egraph_uf.atom_value egraph ~atom with
        | Some false ->
          Hash_set.add t.ext_emitted key;
          let witness = fresh_witness t in
          let literals =
            [ eq left right, true
            ; ( eq
                  (Formula.Select (left, witness))
                  (Formula.Select (right, witness))
              , false )
            ]
          in
          register_lemma_atoms egraph literals;
          t.last_certificate <- Some (Extensionality { left; right; witness });
          Some (`Lemma literals)
        | Some true | None -> None))
;;

let maybe_get_lemma t ~egraph ~get_type =
  t.last_certificate <- None;
  (* Every array lemma needs an array-shaped registered term: the row lemmas
     need a registered [store]/[select (store ...)] term, and extensionality
     additionally requires an array-shaped member in at least one of the two
     classes. [array_terms] records every array-shaped subterm of a registered
     atom, so with it (and any declared array sort) absent no lemma is possible.
     Both are only ever added to and are re-checked on every call, so a term or
     sort that arrives later disables this fast path. *)
  if Hash_set.is_empty t.array_terms && not t.has_declared_array_type
  then `Consistent
  else (
    let terms = Formula_egraph_uf.registered_terms egraph in
    match row1 t egraph terms with
    | Some lemma -> lemma
    | None ->
      (match row2 t egraph terms with
       | Some lemma -> lemma
       | None ->
         (match extensionality t egraph ~get_type with
          | Some lemma -> lemma
          | None -> `Consistent)))
;;

let last_certificate t = t.last_certificate
let undo _ ~to_decision_level_excl:_ = ()
