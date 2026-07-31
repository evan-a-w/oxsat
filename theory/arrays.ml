open! Core
open! Import

module Type_premise = struct
  module T = struct
    type t =
      { var : Tvar.t
      ; type_expr : Type_expr.t
      }
    [@@deriving sexp, compare, hash]
  end

  include T
  include functor Comparable.Make
  include functor Hashable.Make

  let atom { var; type_expr } =
    (Tvar_types.has_type var type_expr :> Atom.Equality.t)
  ;;

  let to_certificate { var; type_expr } = var, type_expr
end

module Ext_key = struct
  module T = struct
    type t =
      { pair : Formula.any list
      ; type_premises : Type_premise.t list
      }
    [@@deriving sexp, compare, hash]
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
  ; ext_emitted : Ext_key.Hash_set.t
  ; mutable last_certificate : Lemma_certificate.Array.t option
  }

let create () =
  { array_terms = Formula.Any.Hash_set.create ()
  ; has_declared_array_type = false
  ; atoms = Atom.Equality.Hash_set.create ()
  ; row1_emitted = Formula.Any.Hash_set.create ()
  ; row2_emitted = Formula.Any.Hash_set.create ()
  ; ext_emitted = Ext_key.Hash_set.create ()
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

let declared_array_type_premise ~get_type = function
  | Formula.Var var ->
    (match get_type var with
     | Some (Type_expr.Array_type _ as type_expr) ->
       Some { Type_premise.var; type_expr }
     | Some
         ( Type_expr.Var _
         | Type_expr.Base _
         | Type_expr.Type_of _
         | Type_expr.App _
         | Type_expr.Function_type _
         | Type_expr.Type )
     | None -> None)
  | _ -> None
;;

module Class_info = struct
  type t =
    { mutable syntactic_array : bool
    ; mutable type_premise : Type_premise.t option
    }

  let create () = { syntactic_array = false; type_premise = None }
  let known_array t = t.syntactic_array || Option.is_some t.type_premise
  let type_guard t = if t.syntactic_array then None else t.type_premise
end

let array_class_info t egraph ~get_type =
  let info_by_repr = Formula.Any.Table.create () in
  List.iter (Formula_egraph_uf.registered_terms egraph) ~f:(fun term ->
    let repr = Formula_egraph_uf.canonical_term egraph ~term in
    let info =
      Hashtbl.find_or_add info_by_repr repr ~default:Class_info.create
    in
    if is_syntactic_array_term t term then info.syntactic_array <- true;
    Option.iter (declared_array_type_premise ~get_type term) ~f:(fun premise ->
      if Option.is_none info.type_premise then info.type_premise <- Some premise));
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

let extensionality_key left right ~type_premises : Ext_key.t =
  { pair = normalized_pair left right
  ; type_premises =
      List.dedup_and_sort type_premises ~compare:Type_premise.compare
  }
;;

let fresh_witness () =
  Formula.Var (Theory_core.Fresh_tvar.create ~hint:"array_extensionality" ())
;;

let extensionality t egraph ~get_type =
  let info_by_repr = array_class_info t egraph ~get_type in
  Hash_set.find_map t.atoms ~f:(function
    | `Type_eq _ -> None
    | `Eq (left, right) as atom ->
      let left_repr, left_info = class_info info_by_repr egraph left in
      let right_repr, right_info = class_info info_by_repr egraph right in
      let both_known_arrays =
        Option.exists left_info ~f:Class_info.known_array
        && Option.exists right_info ~f:Class_info.known_array
      in
      let relevant =
        Option.exists left_info ~f:(fun info -> info.syntactic_array)
        || Option.exists right_info ~f:(fun info -> info.syntactic_array)
      in
      let type_premises =
        List.filter_opt
          [ Option.bind left_info ~f:Class_info.type_guard
          ; Option.bind right_info ~f:Class_info.type_guard
          ]
        |> List.dedup_and_sort ~compare:Type_premise.compare
      in
      let key = extensionality_key left_repr right_repr ~type_premises in
      if Hash_set.mem t.ext_emitted key
         || Formula.equal_any left_repr right_repr
         || not (both_known_arrays && relevant)
      then None
      else (
        match Formula_egraph_uf.atom_value egraph ~atom with
        | Some false ->
          Hash_set.add t.ext_emitted key;
          let witness = fresh_witness () in
          let literals =
            List.map type_premises ~f:(fun premise ->
              Type_premise.atom premise, false)
            @ [ eq left right, true
              ; ( eq
                    (Formula.Select (left, witness))
                    (Formula.Select (right, witness))
                , false )
              ]
          in
          register_lemma_atoms egraph literals;
          t.last_certificate
          <- Some
               (Extensionality
                  { left
                  ; right
                  ; witness
                  ; type_premises =
                      List.map type_premises ~f:Type_premise.to_certificate
                  });
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
