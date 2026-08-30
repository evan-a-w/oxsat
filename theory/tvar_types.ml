open! Core
open! Import
module Type = Type_expr.Base

module Atom = struct
  type t = [ `Has_type of Tvar.t * Type_expr.t ]
  [@@deriving sexp, compare, hash]

  let normalize x = x

  include functor Comparable.Make
  include functor Hashable.Make
end

let has_type var type_expr : Atom.t = `Has_type (var, type_expr)

module Constraint = struct
  type t =
    { decision_level : int
    ; atom : Atom.t
    ; value : bool
    }
  [@@deriving compare]

  let var { atom = `Has_type (var, _); _ } = var
end

type t =
  { constraints_by_var : Constraint.t list Tvar.Table.t
  ; trail : Constraint.t Vec.Value.t
  ; mutable conflict : (Atom.t * bool) list option
  }

let create () =
  { constraints_by_var = Tvar.Table.create ()
  ; trail = Vec.Value.create ()
  ; conflict = None
  }
;;

let meet_types types =
  List.fold_until
    types
    ~init:None
    ~f:(fun acc type_expr ->
      match acc with
      | None -> Continue (Some type_expr)
      | Some acc ->
        (match Type_lattice.meet acc type_expr with
         | Some meet -> Continue (Some meet)
         | None -> Stop None))
    ~finish:Fn.id
;;

let positive_types constraints =
  List.filter_map constraints ~f:(fun ({ atom; value; _ } : Constraint.t) ->
    match atom, value with
    | `Has_type (_, type_expr), true -> Some type_expr
    | `Has_type _, false -> None)
;;

let get_type t var =
  Hashtbl.find t.constraints_by_var var
  |> Option.bind ~f:(fun constraints ->
    let types = positive_types constraints in
    match meet_types types with
    | Some _ as type_ -> type_
    | None -> List.hd types)
;;

let all_typed_vars t =
  Hashtbl.keys t.constraints_by_var
  |> List.filter ~f:(fun var -> Option.is_some (get_type t var))
;;

let positive_conflict constraints =
  let positives =
    List.filter constraints ~f:(fun ({ value; _ } : Constraint.t) -> value)
  in
  List.find_map
    positives
    ~f:
      (fun
        ({ atom = `Has_type (_, left) as left_atom; _ } as left_c :
          Constraint.t)
      ->
      List.find_map
        positives
        ~f:
          (fun
            ({ atom = `Has_type (_, right) as right_atom; _ } as right_c :
              Constraint.t)
          ->
          if Constraint.compare left_c right_c < 0
             && Type_lattice.disjoint left right
          then Some [ left_atom, false; right_atom, false ]
          else None))
;;

let negative_conflict constraints =
  let positives, negatives =
    List.partition_tf constraints ~f:(fun ({ value; _ } : Constraint.t) ->
      value)
  in
  List.find_map
    positives
    ~f:(fun ({ atom = `Has_type (_, sub) as positive; _ } : Constraint.t) ->
      List.find_map
        negatives
        ~f:
          (fun
            ({ atom = `Has_type (_, super) as negative; _ } : Constraint.t) ->
          if Type_lattice.is_subtype sub ~of_:super
          then Some [ positive, false; negative, true ]
          else None))
;;

let recompute_conflict t =
  t.conflict
  <- Hashtbl.data t.constraints_by_var
     |> List.find_map ~f:(fun constraints ->
       match positive_conflict constraints with
       | Some _ as conflict -> conflict
       | None -> negative_conflict constraints)
;;

let assert_atom t ~decision_level ~(atom : Atom.t) ~value =
  let constraint_ = { Constraint.decision_level; atom; value } in
  Vec.Value.push t.trail constraint_;
  Hashtbl.update t.constraints_by_var (Constraint.var constraint_) ~f:(function
    | None -> [ constraint_ ]
    | Some constraints -> constraint_ :: constraints);
  recompute_conflict t
;;

let maybe_get_lemma t =
  match t.conflict with
  | None -> `Consistent
  | Some literals -> `Lemma literals
;;

let remove_first constraints target =
  let rec go rev_prefix = function
    | [] -> List.rev rev_prefix
    | constraint_ :: rest ->
      if [%compare.equal: Constraint.t] constraint_ target
      then List.rev_append rev_prefix rest
      else go (constraint_ :: rev_prefix) rest
  in
  go [] constraints
;;

let undo t ~to_decision_level_excl =
  let rec go () =
    match Vec.Value.last t.trail with
    | Some constraint_ when constraint_.decision_level > to_decision_level_excl
      ->
      let constraint_ = Vec.Value.pop_exn t.trail in
      let var = Constraint.var constraint_ in
      let constraints =
        Hashtbl.find t.constraints_by_var var
        |> Option.value ~default:[]
        |> fun constraints -> remove_first constraints constraint_
      in
      if List.is_empty constraints
      then Hashtbl.remove t.constraints_by_var var
      else Hashtbl.set t.constraints_by_var ~key:var ~data:constraints;
      go ()
    | None | Some _ -> ()
  in
  go ();
  recompute_conflict t
;;
