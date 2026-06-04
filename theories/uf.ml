open! Core
open! Feel
open! Ds
module Term_id = Int

module Term = struct
  module Def = struct
    type t =
      | Const
      | App of
          { func : int
          ; args : Term_id.t array
          }
  end

  type t =
    { id : Term_id.t
    ; def : Def.t
    ; mutable parent : Term_id.t
    ; mutable rank : int
    ; mutable class_list_next : Term_id.t
    }
end

module Atom = struct
  type t =
    | Eq of Term_id.t * Term_id.t
    | Neq of Term_id.t * Term_id.t
  [@@deriving compare, sexp, hash]

  include functor Comparable.Make
  include functor Hashable.Make

  (* Canonical form: smaller id first, so Eq(a,b) = Eq(b,a). *)
  let eq a b = if Term_id.( <= ) a b then Eq (a, b) else Eq (b, a)
  let neq a b = if Term_id.( <= ) a b then Neq (a, b) else Neq (b, a)
end

module Atom_data = struct
  type t = unit
end

module Registry = Atom_registry.Make (Atom) (Atom_data)

module Undo = struct
  type kind =
    | Merge of
        { child : Term_id.t
        ; old_parent : Term_id.t
        ; old_rank : int
        ; reason_literals : int list
        }
    | Class_list of
        { term : Term_id.t
        ; old_next : Term_id.t
        }

  type t =
    { decision_level : int
    ; kind : kind
    }
end

type asserted =
  { a : Term_id.t
  ; b : Term_id.t
  ; literal : int
  ; decision_level : int
  }

type t =
  { terms : Term.t Vec.Value.t
  ; registry : Registry.t
  ; mutable asserted_as_equal : asserted list
  ; mutable asserted_as_disequal : asserted list
  ; mutable undo_trail : Undo.t list
  }

let create () =
  { terms = Vec.Value.create ()
  ; registry = Registry.create ()
  ; asserted_as_equal = []
  ; asserted_as_disequal = []
  ; undo_trail = []
  }
;;

let new_term t ~def =
  let id = Vec.Value.length t.terms in
  Vec.Value.push
    t.terms
    { Term.id; def; parent = id; rank = 0; class_list_next = id };
  id
;;

let new_const t = new_term t ~def:Const
let new_app t ~func ~args = new_term t ~def:(App { func; args })

let register_eq t ~lhs ~rhs =
  Registry.register t.registry ~atom:(Atom.eq lhs rhs) ~data:()
;;

let register_neq t ~lhs ~rhs =
  Registry.register t.registry ~atom:(Atom.neq lhs rhs) ~data:()
;;

let get_term t id = Vec.Value.get t.terms id

let rec find t id =
  let term = get_term t id in
  if term.parent = id then id else find t term.parent
;;

let same_class t a b = Term_id.equal (find t a) (find t b)

let record_undo t ~decision_level kind =
  t.undo_trail <- { Undo.decision_level; kind } :: t.undo_trail
;;

let union t ~decision_level ~reason_literals a b =
  let ra = find t a
  and rb = find t b in
  if Term_id.equal ra rb
  then false
  else (
    let root, child =
      let ta = get_term t ra
      and tb = get_term t rb in
      if ta.rank >= tb.rank then ra, rb else rb, ra
    in
    let child_term = get_term t child in
    let root_term = get_term t root in
    (* Class_list must be pushed before Merge so that on undo (LIFO) the
       class list is restored first, then the parent pointer and rank. *)
    record_undo
      t
      ~decision_level
      (Class_list { term = root; old_next = root_term.class_list_next });
    let old_root_next = root_term.class_list_next in
    root_term.class_list_next <- child_term.class_list_next;
    child_term.class_list_next <- old_root_next;
    record_undo
      t
      ~decision_level
      (Merge
         { child
         ; old_parent = child_term.parent
         ; old_rank = root_term.rank
         ; reason_literals
         });
    child_term.parent <- root;
    if root_term.rank = child_term.rank
    then root_term.rank <- root_term.rank + 1;
    true)
;;

let apply_undo t ({ Undo.decision_level = _; kind } : Undo.t) =
  match kind with
  | Merge { child; old_parent; old_rank; reason_literals = _ } ->
    let child_term = get_term t child in
    let root_term = get_term t child_term.parent in
    root_term.rank <- old_rank;
    child_term.parent <- old_parent
  | Class_list { term; old_next } ->
    (get_term t term).class_list_next <- old_next
;;

let on_new_var t ~var = Registry.on_new_var t.registry var

let drop_above_level list ~to_decision_level =
  (* The list is maintained in newest-first order, so entries above the target
     level form a contiguous prefix — drop them with a single pass. *)
  let rec go = function
    | entry :: rest when entry.decision_level > to_decision_level -> go rest
    | remaining -> remaining
  in
  go list
;;

let pop t ~to_decision_level =
  let rec go = function
    | entry :: rest when entry.Undo.decision_level > to_decision_level ->
      apply_undo t entry;
      go rest
    | remaining -> t.undo_trail <- remaining
  in
  go t.undo_trail;
  t.asserted_as_equal <- drop_above_level t.asserted_as_equal ~to_decision_level;
  t.asserted_as_disequal
  <- drop_above_level t.asserted_as_disequal ~to_decision_level
;;

let assert_literal t ~decision_level literal =
  let var = Int.abs literal in
  let is_positive = literal > 0 in
  match Registry.atom t.registry ~var with
  | Null -> ()
  | This atom ->
    (match atom, is_positive with
     | Atom.Eq (a, b), true ->
       (* eq-var is true: a = b *)
       t.asserted_as_equal
       <- { a; b; literal; decision_level } :: t.asserted_as_equal;
       ignore (union t ~decision_level ~reason_literals:[ literal ] a b : bool)
     | Atom.Neq (a, b), false ->
       (* neq-var is false, i.e. ¬(a≠b): a = b *)
       t.asserted_as_equal
       <- { a; b; literal; decision_level } :: t.asserted_as_equal;
       ignore (union t ~decision_level ~reason_literals:[ literal ] a b : bool)
     | Atom.Neq (a, b), true ->
       (* neq-var is true: a ≠ b *)
       t.asserted_as_disequal
       <- { a; b; literal; decision_level } :: t.asserted_as_disequal
     | Atom.Eq (a, b), false ->
       (* eq-var is false, i.e. ¬(a=b): a ≠ b *)
       t.asserted_as_disequal
       <- { a; b; literal; decision_level } :: t.asserted_as_disequal)
;;

let decision_level_of_literal t literal =
  let matching (asserted : asserted) = asserted.literal = literal in
  match List.find t.asserted_as_equal ~f:matching with
  | Some asserted -> asserted.decision_level
  | None ->
    (match List.find t.asserted_as_disequal ~f:matching with
     | Some asserted -> asserted.decision_level
     | None ->
       Error.raise_s
         [%message
           "BUG: UF explanation references a literal that is not asserted"
             (literal : int)])
;;

let decision_level_of_premises t literals =
  List.fold literals ~init:0 ~f:(fun acc literal ->
    Int.max acc (decision_level_of_literal t literal))
;;

let unique_literals literals = Set.to_list (Int.Set.of_list literals)

(* Walk the union-find parent-pointer path from [id] up to [root], collecting
   the reason literals of each merge along the way. There is no path
   compression, so each [child]'s parent is set by exactly one active Merge
   entry in the undo trail. *)
let explain_path_to_root t id root =
  let rec go id acc =
    if Term_id.equal id root
    then acc
    else (
      let current_parent = (get_term t id).parent in
      let reason =
        List.find_map t.undo_trail ~f:(fun { Undo.kind; _ } ->
          match kind with
          | Merge { child; reason_literals; _ } when Term_id.equal child id ->
            Some reason_literals
          | _ -> None)
      in
      let acc =
        match reason with
        | None -> acc
        | Some lits -> lits @ acc
      in
      go current_parent acc)
  in
  go id []
;;

(* Collect the reason literals that justify [a] and [b] being in the same
   equivalence class by walking both paths to their common root. *)
let explain_eq t a b =
  let root = find t a in
  assert (Term_id.equal root (find t b));
  (explain_path_to_root t a root @ explain_path_to_root t b root)
  |> unique_literals
;;

let registered_eq_var t lhs rhs =
  Or_null.to_option (Registry.var t.registry ~atom:(Atom.eq lhs rhs))
;;

let registered_neq_var t lhs rhs =
  Or_null.to_option (Registry.var t.registry ~atom:(Atom.neq lhs rhs))
;;

let congruent t ta tb =
  match ta.Term.def, tb.Term.def with
  | App { func = fa; args = aa }, App { func = fb; args = ab } ->
    Int.equal fa fb
    && Array.length aa = Array.length ab
    && Array.for_all2_exn aa ab ~f:(fun a b -> same_class t a b)
  | _ -> false
;;

let congruence_reason t ti tj =
  match ti.Term.def, tj.Term.def with
  | App { args = aa; _ }, App { args = ab; _ } ->
    Array.to_list aa
    |> List.concat_mapi ~f:(fun k arg_i -> explain_eq t arg_i ab.(k))
    |> unique_literals
  | _ -> []
;;

let close_congruence t =
  let n = Vec.Value.length t.terms in
  let changed = ref true in
  while !changed do
    changed := false;
    for i = 0 to n - 1 do
      let ti = get_term t i in
      match ti.Term.def with
      | Const -> ()
      | App _ ->
        for j = i + 1 to n - 1 do
          let tj = get_term t j in
          if congruent t ti tj && not (same_class t i j)
          then (
            let reason_literals = congruence_reason t ti tj in
            let decision_level = decision_level_of_premises t reason_literals in
            if union t ~decision_level ~reason_literals i j then changed := true)
        done
    done
  done
;;

let conflict_for_asserted_disequality t =
  List.find_map t.asserted_as_disequal ~f:(fun { a; b; literal; _ } ->
    if same_class t a b
    then (
      let premises = explain_eq t a b in
      Some (Array.of_list (-literal :: List.map premises ~f:(fun lit -> -lit))))
    else None)
;;

let propagation_clause ~literal ~premises =
  Array.of_list (literal :: List.map premises ~f:(fun premise -> -premise))
;;

let var_is_asserted t ~var =
  List.exists t.asserted_as_equal ~f:(fun e -> Int.equal (Int.abs e.literal) var)
  || List.exists t.asserted_as_disequal ~f:(fun e ->
       Int.equal (Int.abs e.literal) var)
;;

let check_consistent t =
  close_congruence t;
  match conflict_for_asserted_disequality t with
  | Some clause -> `Conflict clause
  | None ->
    let n = Vec.Value.length t.terms in
    let propagations = ref [] in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        if same_class t i j
        then (
          let premises = explain_eq t i j in
          Option.iter (registered_eq_var t i j) ~f:(fun var ->
            if not (var_is_asserted t ~var)
            then
              propagations
              := (var, propagation_clause ~literal:var ~premises)
                 :: !propagations);
          Option.iter (registered_neq_var t i j) ~f:(fun var ->
            if not (var_is_asserted t ~var)
            then
              propagations
              := (-var, propagation_clause ~literal:(-var) ~premises)
                 :: !propagations))
      done
    done;
    (match !propagations with
     | [] -> `Consistent
     | ps -> `Propagate ps)
;;
