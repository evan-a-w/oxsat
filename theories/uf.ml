open! Core
open! Feel
open! Ds

(* Uninterpreted functions theory for DPLL(T).

   Terms are integer IDs. Atoms are equalities (t1 = t2) or disequalities
   (t1 != t2), each associated with a Boolean variable in the SAT solver.

   The theory maintains a union-find over terms, updated incrementally as the
   SAT solver asserts literals. On [check_consistent] it detects congruence
   conflicts and propagates implied equalities.

   [pop ~to_decision_level] unwinds the union-find to the state it was in at
   that level using a per-level undo trail. *)

(* ───────────────────────── Term representation ───────────────────────────── *)

module Term_id = Int

(* A term is either a constant/variable, or a function application. *)
type term_def =
  | Const
  | App of { func : int; args : Term_id.t array }

type term =
  { id : Term_id.t
  ; def : term_def
  ; mutable parent : Term_id.t
  ; mutable rank : int
  (* Other terms in the same equivalence class that are watched for congruence *)
  ; mutable class_list_next : Term_id.t
  }

(* ──────────────────────────── Atom registry ─────────────────────────────── *)

type atom_kind =
  | Eq of Term_id.t * Term_id.t
  | Neq of Term_id.t * Term_id.t

type atom =
  { var : int
  ; kind : atom_kind
  }

(* ───────────────────────── Undo trail entry ─────────────────────────────── *)

type undo_entry =
  { decision_level : int
  ; kind : undo_kind
  }

and undo_kind =
  (* union-find merge: child's parent was changed to root *)
  | Merge of { child : Term_id.t; old_parent : Term_id.t; old_rank : int }
  (* class list pointer was changed *)
  | Class_list of { term : Term_id.t; old_next : Term_id.t }

(* ─────────────────────────── Theory state ───────────────────────────────── *)

type t =
  { terms : term Vec.Value.t
  ; atoms : atom Vec.Value.t
  (* map from SAT variable to atom index *)
  ; var_to_atom : int Int.Table.t
  (* currently asserted equalities and disequalities *)
  ; mutable asserted_eqs : (Term_id.t * Term_id.t * int (* var *)) list
  ; mutable asserted_neqs : (Term_id.t * Term_id.t * int (* var *)) list
  (* undo trail in reverse chronological order *)
  ; mutable undo_trail : undo_entry list
  }

let create () =
  { terms = Vec.Value.create ()
  ; atoms = Vec.Value.create ()
  ; var_to_atom = Int.Table.create ()
  ; asserted_eqs = []
  ; asserted_neqs = []
  ; undo_trail = []
  }
;;

(* ─────────────────────────── Public API ────────────────────────────────── *)

let new_term t ~def =
  let id = Vec.Value.length t.terms in
  Vec.Value.push
    t.terms
    { id; def; parent = id; rank = 0; class_list_next = id };
  id
;;

let new_const t = new_term t ~def:Const
let new_app t ~func ~args = new_term t ~def:(App { func; args })

(* Register a Boolean variable as representing an equality atom. Returns the
   variable back for convenience when building the formula. *)
let register_eq t ~var ~lhs ~rhs =
  let idx = Vec.Value.length t.atoms in
  Vec.Value.push t.atoms { var; kind = Eq (lhs, rhs) };
  Hashtbl.set t.var_to_atom ~key:var ~data:idx;
  var
;;

let register_neq t ~var ~lhs ~rhs =
  let idx = Vec.Value.length t.atoms in
  Vec.Value.push t.atoms { var; kind = Neq (lhs, rhs) };
  Hashtbl.set t.var_to_atom ~key:var ~data:idx;
  var
;;

(* ─────────────────────────── Union-find ────────────────────────────────── *)

let get_term t id = Vec.Value.get t.terms id

let rec find t id =
  let term = get_term t id in
  if term.parent = id then id else find t term.parent
;;

let same_class t a b = Term_id.equal (find t a) (find t b)

let record_undo t ~decision_level kind =
  t.undo_trail <- { decision_level; kind } :: t.undo_trail
;;

(* Merge the equivalence classes of [a] and [b]. Returns false if already
   merged. Updates class lists for congruence checking. *)
let union t ~decision_level a b =
  let ra = find t a and rb = find t b in
  if Term_id.equal ra rb
  then false
  else (
    let root, child =
      let ta = get_term t ra and tb = get_term t rb in
      if ta.rank >= tb.rank then ra, rb else rb, ra
    in
    let child_term = get_term t child in
    let root_term = get_term t root in
    record_undo
      t
      ~decision_level
      (Merge { child; old_parent = child_term.parent; old_rank = root_term.rank });
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

(* ─────────────────────── Undo / pop ────────────────────────────────────── *)

let apply_undo t { decision_level = _; kind } =
  match kind with
  | Merge { child; old_parent; old_rank } ->
    let child_term = get_term t child in
    let root_term = get_term t child_term.parent in
    root_term.rank <- old_rank;
    child_term.parent <- old_parent
  | Class_list { term; old_next } ->
    (get_term t term).class_list_next <- old_next
;;

let on_new_var _t ~var:_ = ()

let pop t ~to_decision_level =
  let rec go = function
    | entry :: rest when entry.decision_level > to_decision_level ->
      apply_undo t entry;
      go rest
    | remaining -> t.undo_trail <- remaining
  in
  go t.undo_trail;
  t.asserted_eqs
  <- List.filter t.asserted_eqs ~f:(fun (a, b, _var) -> same_class t a b);
  t.asserted_neqs
  <- List.filter t.asserted_neqs ~f:(fun (a, b, _var) ->
    not (same_class t a b))
;;

(* ──────────────────────────── assert_literal ───────────────────────────── *)

let assert_literal t ~decision_level literal =
  let var = Int.abs literal in
  let is_positive = literal > 0 in
  match Hashtbl.find t.var_to_atom var with
  | None -> ()
  | Some idx ->
    let atom = Vec.Value.get t.atoms idx in
    (match atom.kind, is_positive with
     | Eq (a, b), true ->
       t.asserted_eqs <- (a, b, var) :: t.asserted_eqs;
       ignore (union t ~decision_level a b : bool)
     | Neq (a, b), false ->
       (* negative literal on a neq atom means the equality holds *)
       t.asserted_eqs <- (a, b, var) :: t.asserted_eqs;
       ignore (union t ~decision_level a b : bool)
     | Neq (a, b), true ->
       t.asserted_neqs <- (a, b, var) :: t.asserted_neqs
     | Eq (a, b), false ->
       t.asserted_neqs <- (a, b, var) :: t.asserted_neqs)
;;

(* ──────────────────────── Congruence helpers ───────────────────────────── *)

(* Walk the class list of the representative of [id], returning all term IDs
   in the equivalence class. *)
let class_members t id =
  let root = find t id in
  let root_term = get_term t root in
  let members = ref [] in
  let cur = ref root in
  let continue_ = ref true in
  while !continue_ do
    members := !cur :: !members;
    let next = (get_term t !cur).class_list_next in
    if Term_id.equal next root_term.id then continue_ := false else cur := next
  done;
  !members
;;

(* Check if two App terms are congruent (same function, all args in same class) *)
let congruent t ta tb =
  match ta.def, tb.def with
  | App { func = fa; args = aa }, App { func = fb; args = ab } ->
    Int.equal fa fb
    && Array.length aa = Array.length ab
    && Array.for_all2_exn aa ab ~f:(fun a b -> same_class t a b)
  | _ -> false
;;

(* Find an atom (equality variable) witnessing that lhs = rhs, if any
   registered. Returns the variable (positive = eq atom, negative = neq atom
   negated). *)
let find_eq_atom t lhs rhs =
  Vec.Value.findi t.atoms ~f:(fun _i atom ->
    match atom.kind with
    | Eq (a, b)
      when (Term_id.equal a lhs && Term_id.equal b rhs)
           || (Term_id.equal a rhs && Term_id.equal b lhs) -> Some atom.var
    | Neq (a, b)
      when (Term_id.equal a lhs && Term_id.equal b rhs)
           || (Term_id.equal a rhs && Term_id.equal b lhs) ->
      Some (-atom.var)
    | _ -> None)
;;

(* ──────────────────────────── check_consistent ─────────────────────────── *)

let check_consistent t =
  (* 1. Check for disequality conflicts: a != b but find(a) = find(b) *)
  let conflict =
    List.find_map t.asserted_neqs ~f:(fun (a, b, neq_var) ->
      if same_class t a b
      then (
        (* Build conflict clause: negate all equalities that caused the merge.
           The clause is: NOT (all the equalities that made a=b AND a!=b).
           Minimally: [-eq1, -eq2, ..., neq_var] where eq_i are the asserted
           equalities on the path. For simplicity we use all asserted_eqs and
           the neq literal. *)
        let eq_lits =
          List.filter_map t.asserted_eqs ~f:(fun (_a, _b, eq_var) ->
            Some (-eq_var))
        in
        Some (Array.of_list (neq_var :: eq_lits)))
      else None)
  in
  (match conflict with
   | Some clause -> `Conflict clause
   | None ->
     (* 2. Check for congruence propagations: f(a,b) and f(a',b') where
        a~a' and b~b', so f(a,b)~f(a',b') should be propagated. *)
     let n = Vec.Value.length t.terms in
     let propagations = ref [] in
     for i = 0 to n - 1 do
       let ti = get_term t i in
       (match ti.def with
        | Const -> ()
        | App _ ->
          for j = i + 1 to n - 1 do
            let tj = get_term t j in
            if congruent t ti tj && not (same_class t i j)
            then (
              (* i and j should be equal by congruence. Find or build the
                 explanation clause and a literal to propagate. *)
              match find_eq_atom t i j with
              | None -> ()
              | Some lit when lit > 0 ->
                (* There's a registered eq atom for i=j; propagate it true.
                   Explanation: the congruence holds because of asserted eqs
                   on the args. For now use all asserted_eqs as the reason —
                   a more precise implementation would walk the proof. *)
                let reason =
                  Array.of_list
                    (lit
                     :: List.filter_map t.asserted_eqs ~f:(fun (_a, _b, eq_var) ->
                       Some (-eq_var)))
                in
                propagations := (lit, reason) :: !propagations
              | Some _ -> ())
          done)
     done;
     (match !propagations with
      | [] -> `Consistent
      | ps -> `Propagate ps))
;;
