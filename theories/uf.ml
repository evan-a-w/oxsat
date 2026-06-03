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

  let create_unregistered () = None

  (* Canonical form: smaller id first, so Eq(a,b) = Eq(b,a). *)
  let eq a b = if Term_id.( <= ) a b then Eq (a, b) else Eq (b, a)
  let neq a b = if Term_id.( <= ) a b then Neq (a, b) else Neq (b, a)
end

module Atom_data = struct
  type t = unit

  let create_unregistered () = ()
end

module Registry = Atom_registry.Make (Atom) (Atom_data)

module Undo = struct
  type kind =
    | Merge of
        { child : Term_id.t
        ; old_parent : Term_id.t
        ; old_rank : int
        (* The eq var that caused this merge, for conflict explanation. *)
        ; reason_var : int
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
  ; var : int
  ; decision_level : int
  }

type t =
  { terms : Term.t Vec.Value.t
  ; registry : Registry.t
  ; mutable asserted_eqs : asserted list
  ; mutable asserted_neqs : asserted list
  ; mutable undo_trail : Undo.t list
  }

let create () =
  { terms = Vec.Value.create ()
  ; registry = Registry.create ()
  ; asserted_eqs = []
  ; asserted_neqs = []
  ; undo_trail = []
  }
;;

let new_term t ~def =
  let id = Vec.Value.length t.terms in
  Vec.Value.push t.terms { Term.id; def; parent = id; rank = 0; class_list_next = id };
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

let union t ~decision_level ~reason_var a b =
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
    record_undo
      t
      ~decision_level
      (Merge { child; old_parent = child_term.parent; old_rank = root_term.rank; reason_var });
    child_term.parent <- root;
    if root_term.rank = child_term.rank then root_term.rank <- root_term.rank + 1;
    record_undo
      t
      ~decision_level
      (Class_list { term = root; old_next = root_term.class_list_next });
    let old_root_next = root_term.class_list_next in
    root_term.class_list_next <- child_term.class_list_next;
    child_term.class_list_next <- old_root_next;
    true)
;;

let apply_undo t ({ Undo.decision_level = _; kind } : Undo.t) =
  match kind with
  | Merge { child; old_parent; old_rank; reason_var = _ } ->
    let child_term = get_term t child in
    let root_term = get_term t child_term.parent in
    root_term.rank <- old_rank;
    child_term.parent <- old_parent
  | Class_list { term; old_next } ->
    (get_term t term).class_list_next <- old_next
;;

let on_new_var t ~var = Registry.on_new_var t.registry var

let pop t ~to_decision_level =
  let rec go = function
    | entry :: rest when entry.Undo.decision_level > to_decision_level ->
      apply_undo t entry;
      go rest
    | remaining -> t.undo_trail <- remaining
  in
  go t.undo_trail;
  t.asserted_eqs
  <- List.filter t.asserted_eqs ~f:(fun e -> e.decision_level <= to_decision_level);
  t.asserted_neqs
  <- List.filter t.asserted_neqs ~f:(fun e -> e.decision_level <= to_decision_level)
;;

let assert_literal t ~decision_level literal =
  let var = Int.abs literal in
  let is_positive = literal > 0 in
  match Registry.atom t.registry ~var with
  | Null -> ()
  | This atom ->
    (match atom, is_positive with
     | Atom.Eq (a, b), true ->
       t.asserted_eqs <- { a; b; var; decision_level } :: t.asserted_eqs;
       ignore (union t ~decision_level ~reason_var:var a b : bool)
     | Atom.Neq (a, b), false ->
       t.asserted_eqs <- { a; b; var; decision_level } :: t.asserted_eqs;
       ignore (union t ~decision_level ~reason_var:var a b : bool)
     | Atom.Neq (a, b), true ->
       t.asserted_neqs <- { a; b; var; decision_level } :: t.asserted_neqs
     | Atom.Eq (a, b), false ->
       t.asserted_neqs <- { a; b; var; decision_level } :: t.asserted_neqs)
;;

(* Walk the undo trail to collect the reason vars for all merges that put
   [a] and [b] into the same class. Returns a list of eq vars. *)
let explain_eq t a b =
  (* We need the vars on the merge path. The undo trail records every merge
     in reverse chronological order; the merges that matter are those whose
     child's current root is the same as find(a) or find(b). A simple
     sound (but possibly over-approximate) approach: collect all Merge
     reason_vars for the two roots involved. For soundness we include all
     merges that contributed to the current class of find(a). *)
  let root = find t a in
  assert (Term_id.equal root (find t b));
  List.filter_map t.undo_trail ~f:(fun { Undo.kind; _ } ->
    match kind with
    | Merge { child; reason_var; _ } ->
      if Term_id.equal (find t child) root then Some reason_var else None
    | Class_list _ -> None)
;;

let find_eq_var t lhs rhs =
  match Registry.var t.registry ~atom:(Atom.eq lhs rhs) with
  | This v -> Some v
  | Null ->
    (match Registry.var t.registry ~atom:(Atom.neq lhs rhs) with
     | This v -> Some (-v)
     | Null -> None)
;;

let congruent t ta tb =
  match ta.Term.def, tb.Term.def with
  | App { func = fa; args = aa }, App { func = fb; args = ab } ->
    Int.equal fa fb
    && Array.length aa = Array.length ab
    && Array.for_all2_exn aa ab ~f:(fun a b -> same_class t a b)
  | _ -> false
;;

let check_consistent t =
  let conflict =
    List.find_map t.asserted_neqs ~f:(fun { a; b; var = neq_var; _ } ->
      if same_class t a b
      then (
        let path_vars = explain_eq t a b in
        Some (Array.of_list (neq_var :: List.map path_vars ~f:(fun v -> -v))))
      else None)
  in
  match conflict with
  | Some clause -> `Conflict clause
  | None ->
    let n = Vec.Value.length t.terms in
    let propagations = ref [] in
    for i = 0 to n - 1 do
      let ti = get_term t i in
      match ti.Term.def with
      | Const -> ()
      | App _ ->
        for j = i + 1 to n - 1 do
          let tj = get_term t j in
          if congruent t ti tj && not (same_class t i j)
          then (
            match find_eq_var t i j with
            | None -> ()
            | Some lit when lit > 0 ->
              let arg_eq_vars =
                match ti.Term.def, tj.Term.def with
                | App { args = aa; _ }, App { args = ab; _ } ->
                  Array.to_list aa
                  |> List.concat_mapi ~f:(fun k arg_i ->
                    explain_eq t arg_i ab.(k) |> List.map ~f:(fun v -> -v))
                | _ -> []
              in
              let reason = Array.of_list (lit :: arg_eq_vars) in
              propagations := (lit, reason) :: !propagations
            | Some _ -> ())
        done
    done;
    (match !propagations with
     | [] -> `Consistent
     | ps -> `Propagate ps)
;;
