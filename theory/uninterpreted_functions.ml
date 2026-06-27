open! Core
open! Feel.Import
include Uninterpreted_functions_intf

module Make (Term : Term) = struct
  module Atom = struct
    type t = [ `Eq of Term.t * Term.t ] [@@deriving sexp, compare, hash]

    let normalize = function
      | `Eq (a, b) as x ->
        (match Ordering.of_int ([%compare: Term.t] a b) with
         | Equal | Less -> x
         | Greater -> `Eq (b, a))
    ;;

    let garbage_for_vec = `Eq (Term.garbage_for_vec, Term.garbage_for_vec)

    include functor Comparable.Make
    include functor Hashable.Make
  end

  module Atom_data = struct
    type t =
      { atom : Atom.t
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

  (* Justification for an edge in the [Explanation_forest]: either a directly
     asserted equality atom, or a congruence step [f(args1) ~ f(args2)] that
     reduces to pairwise equalities between corresponding arguments. *)
  module Justification = struct
    type t =
      | Asserted of Atom.t
      | Congruence of
          { function_ : Tvar.t
          ; args1 : Term.t list
          ; args2 : Term.t list
          }
    [@@deriving sexp_of]
  end

  module Term_pair = struct
    type t = Term.t * Term.t [@@deriving sexp, compare, hash]

    include functor Hashable.Make
  end

  (* A spanning forest over ufds vars that mirrors the union-find's merge
     history (à la Kruskal's algorithm: every successful union connects two
     previously-distinct classes, so recording an edge at union time yields a
     forest whose connectivity exactly matches class equivalence). This lets us
     reconstruct *why* two terms ended up in the same class, which [Ufdsu]
     itself cannot do. Edges are added/removed in strict LIFO order with
     merges/undos, so [Hashtbl.add_multi]/[remove_multi] (which cons/pop list
     heads) keep both endpoints' adjacency lists correctly synchronized. *)
  module Explanation_forest = struct
    type t = (int * Justification.t) list Int.Table.t

    let create () : t = Int.Table.create ()

    let add_edge (t : t) ~x ~y ~justification =
      Hashtbl.add_multi t ~key:x ~data:(y, justification);
      Hashtbl.add_multi t ~key:y ~data:(x, justification)
    ;;

    let remove_edge (t : t) ~x ~y =
      Hashtbl.remove_multi t x;
      Hashtbl.remove_multi t y
    ;;

    let neighbors (t : t) x = Hashtbl.find_multi t x
  end

  module Trail_entry = struct
    module Kind = struct
      type (_ : (value & value & value) & value) tag =
        | Undo : #(Ufdsu.Undo_entry.t * Atom.t) tag
        | Falsehood : #(#(Atom.t * unit * unit) * unit) tag
        | Truth : #(#(Atom.t * unit * unit) * unit) tag
        | Signature_added : #(#(Signature.t * unit * unit) * unit) tag
        | Signature_changed :
            #(#(Signature.t * old_term:Term.t * unit) * unit) tag

      type t = T : #('a tag * 'a) -> t [@@unboxed]

      let undo ~(undo_entry : Ufdsu.Undo_entry.t) ~atom =
        T #(Undo, #(undo_entry, atom))
      ;;

      let falsehood ~atom = T #(Falsehood, #(#(atom, (), ()), ()))
      let truth ~atom = T #(Truth, #(#(atom, (), ()), ()))

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
             ~atom:Atom.garbage_for_vec
       ; decision_level = 0
       }
    ;;

    let create_for_vec () = fake

    include
      functor
      Vecable.Make
      [@kind (value & (value & value & value) & value) & value]
  end

  type t =
    { ufds_var_by_term : int Term.Table.t
    ; ufdsu : Ufdsu.t
    ; atoms : Atom_data.t Atom.Table.t
    ; signature_to_canonical : Term.t Signature.Table.t
    ; parents : Term.Hash_set.t Term.Table.t
    ; explanation_forest : Explanation_forest.t
    ; trail : Trail_entry.Vec.t
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
          | T #(Falsehood, #(#(atom, (), ()), ())) ->
            Hash_set.remove t.falsehoods atom;
            (Hashtbl.find_exn t.atoms atom).assignment <- Null
          | T #(Truth, #(#(atom, (), ()), ())) ->
            (Hashtbl.find_exn t.atoms atom).assignment <- Null
          | T #(Signature_added, #(#(signature, (), ()), ())) ->
            Hashtbl.remove t.signature_to_canonical signature
          | T #(Signature_changed, #(#(signature, ~old_term, ()), ())) ->
            Hashtbl.set t.signature_to_canonical ~key:signature ~data:old_term
          | T #(Undo, #(undo_entry, _)) ->
            Explanation_forest.remove_edge
              t.explanation_forest
              ~x:undo_entry.#child
              ~y:undo_entry.#new_root;
            Ufdsu.undo t.ufdsu ~undo_entry);
         undo t ~to_decision_level_excl)
  ;;

  let ufds_var t ~term = Hashtbl.find_exn t.ufds_var_by_term term
  let canonical_ufds_var t ~term = ufds_var t ~term |> Ufdsu.find t.ufdsu _

  let signature t ~(term : Term.t) : Signature.t or_null =
    match Term.split_function term with
    | None -> Null
    | Some (function_, args) ->
      let args = List.map args ~f:(fun term -> canonical_ufds_var t ~term) in
      This { function_; args }
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

  (* Processes [worklist] (pairs of terms known to be congruent, with the
     justification for why), unioning their ufds classes and propagating any
     resulting congruences transitively (e.g. merging [a]/[b] may make
     [f(a)]/[f(b)] congruent, which is pushed back onto the worklist). Used both
     when an equality atom is asserted true (seeded with that atom's two sides)
     and when a newly-registered term's signature already matches an existing
     canonical term (seeded with that pair). *)
  let propagate_congruence t ~decision_level ~(atom : Atom.t) ~worklist =
    let rec go () =
      match Vec.Value.length worklist with
      | 0 -> ()
      | _ ->
        let (term1, term2), justification = Vec.Value.pop_exn worklist in
        let t1 = canonical_ufds_var t ~term:term1 in
        let t2 = canonical_ufds_var t ~term:term2 in
        (match Ufdsu.same_class t.ufdsu t1 t2 with
         | true -> ()
         | false ->
           (* TODO: ideally calculation of [could_change] is done more
              efficiently *)
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
           Explanation_forest.add_edge
             t.explanation_forest
             ~x:t1
             ~y:t2
             ~justification;
           Trail_entry.Vec.push
             t.trail
             #{ decision_level; kind = Trail_entry.Kind.undo ~undo_entry ~atom };
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
                  (* need to merge: [term] and [canonical] have matching
                     signatures, i.e. [f(args1) ~ f(args2)] with [args1]/[args2]
                     pairwise congruent (already in the same class). *)
                  let justification =
                    match (term : Term.t), (canonical : Term.t) with
                    | ( `App (~function_, ~args:args1)
                      , `App (~function_:_, ~args:args2) ) ->
                      Justification.Congruence { function_; args1; args2 }
                    | `Var _, _ | _, `Var _ ->
                      (* [canonical_signature_term] only ever returns [This _]
                         for terms with non-null signatures, i.e. [`App _]. *)
                      assert false
                  in
                  Vec.Value.push worklist ((term, canonical), justification)));
           go ())
    in
    go ()
  ;;

  (* Registers [term] (and, transitively, the [parents] links from its arguments
     to it) in the union-find and term tables, if not already present. If [term]
     is an [App] whose signature (under the current union-find) already matches
     a different canonical term, propagates the resulting congruence (e.g.
     registering [f(x)] after [x ~ y] is already known, with [f(y)] already
     registered, immediately merges [f(x)] and [f(y)]). Used by both [create]
     and [add_atom]. *)
  let rec register_term t ~decision_level ~(atom : Atom.t) ~(term : Term.t) =
    match Hashtbl.find t.ufds_var_by_term term with
    | Some _ -> ()
    | None ->
      (match term with
       | `Var _ -> ()
       | `App (~function_:_, ~args) ->
         List.iter args ~f:(fun arg ->
           register_term t ~decision_level ~atom ~term:arg));
      let ufds_var = Ufdsu.add t.ufdsu in
      Hashtbl.set t.ufds_var_by_term ~key:term ~data:ufds_var;
      (match term with
       | `Var _ -> ()
       | `App (~function_, ~args) ->
         List.iter args ~f:(fun arg ->
           let parents =
             Hashtbl.find_or_add t.parents arg ~default:Term.Hash_set.create
           in
           Hash_set.add parents term);
         (match canonical_signature_term t ~term with
          | Null -> ()
          | This canonical when [%equal: Term.t] canonical term -> ()
          | This canonical ->
            let justification =
              match (canonical : Term.t) with
              | `App (~function_:_, ~args:args2) ->
                Justification.Congruence { function_; args1 = args; args2 }
              | `Var _ ->
                (* [canonical_signature_term] only ever returns [This _] for
                   terms with non-null signatures, i.e. [`App _]. *)
                assert false
            in
            let worklist = Vec.Value.create () in
            Vec.Value.push worklist ((term, canonical), justification);
            propagate_congruence t ~decision_level ~atom ~worklist))
  ;;

  (* Registers [atom] (and its terms). Used by both [create] and [add_atom]. *)
  let register_atom t ~(atom : Atom.t) =
    let atom = Atom.normalize atom in
    let (`Eq (a, b)) = atom in
    let decision_level = t.current_decision_level in
    register_term t ~decision_level ~atom ~term:a;
    register_term t ~decision_level ~atom ~term:b;
    Hashtbl.set t.atoms ~key:atom ~data:{ atom; assignment = Null }
  ;;

  let create ~atoms =
    let t =
      { ufds_var_by_term = Term.Table.create ()
      ; ufdsu = Ufdsu.create ()
      ; atoms = Atom.Table.create ()
      ; parents = Term.Table.create ()
      ; explanation_forest = Explanation_forest.create ()
      ; trail = Trail_entry.Vec.create ()
      ; signature_to_canonical = Signature.Table.create ()
      ; falsehoods = Atom.Hash_set.create ()
      ; current_decision_level = 0
      }
    in
    List.iter atoms ~f:(fun atom -> register_atom t ~atom);
    t
  ;;

  let add_atom t ~atom = register_atom t ~atom

  let assert_true_atom t ~decision_level ~(atom : Atom.t) =
    match Hashtbl.find_or_null t.atoms atom with
    | Null -> ()
    | This atom_data ->
      atom_data.assignment <- This true;
      Trail_entry.Vec.push
        t.trail
        #{ decision_level; kind = Trail_entry.Kind.truth ~atom };
      let worklist = Vec.Value.create () in
      let (`Eq (a, b)) = atom in
      Vec.Value.push worklist ((a, b), Justification.Asserted atom);
      propagate_congruence t ~decision_level ~atom ~worklist
  ;;

  let assert_false_atom t ~decision_level ~(atom : Atom.t) =
    match Hashtbl.find_or_null t.atoms atom with
    | Null -> ()
    | This atom_data ->
      (match Hash_set.mem t.falsehoods atom with
       | true -> ()
       | false ->
         Hash_set.add t.falsehoods atom;
         atom_data.assignment <- This false;
         Trail_entry.Vec.push
           t.trail
           #{ decision_level; kind = Trail_entry.Kind.falsehood ~atom })
  ;;

  let assert_atom t ~decision_level ~(atom : Atom.t) ~value =
    t.current_decision_level <- decision_level;
    let atom = Atom.normalize atom in
    match value with
    | true -> assert_true_atom t ~decision_level ~atom
    | false -> assert_false_atom t ~decision_level ~atom
  ;;

  (* BFS over the [Explanation_forest] from [from] to [to_] (both ufds vars in
     the same class), returning the chain of justifications along the unique
     path connecting them. *)
  let explanation_path t ~from ~to_ =
    match from = to_ with
    | true -> []
    | false ->
      let visited = Int.Hash_set.create () in
      let came_from : (int * Justification.t) Int.Table.t =
        Int.Table.create ()
      in
      let queue = Queue.create () in
      Hash_set.add visited from;
      Queue.enqueue queue from;
      let rec bfs () =
        match Queue.dequeue queue with
        | None -> ()
        | Some node ->
          List.iter
            (Explanation_forest.neighbors t.explanation_forest node)
            ~f:(fun (neighbor, justification) ->
              match Hash_set.mem visited neighbor with
              | true -> ()
              | false ->
                Hash_set.add visited neighbor;
                Hashtbl.set came_from ~key:neighbor ~data:(node, justification);
                Queue.enqueue queue neighbor);
          bfs ()
      in
      bfs ();
      let rec build acc node =
        match node = from with
        | true -> acc
        | false ->
          let prev, justification = Hashtbl.find_exn came_from node in
          build (justification :: acc) prev
      in
      build [] to_
  ;;

  (* Recursively collects, into [facts], every directly-asserted equality atom
     used to prove [term1 ~ term2] under the current assignment. Congruence
     steps [f(args1) ~ f(args2)] are expanded into pairwise argument equalities;
     [seen] memoizes term pairs already explained so that shared subterms (DAG
     sharing) don't cause exponential blowup. *)
  let rec explain t ~term1 ~term2 ~seen ~facts =
    let v1 = ufds_var t ~term:term1 in
    let v2 = ufds_var t ~term:term2 in
    match v1 = v2 with
    | true -> ()
    | false ->
      let path = explanation_path t ~from:v1 ~to_:v2 in
      List.iter path ~f:(function
        | Justification.Asserted atom -> Hash_set.add facts atom
        | Justification.Congruence { function_ = _; args1; args2 } ->
          List.iter2_exn args1 args2 ~f:(fun a b ->
            let key : Term_pair.t =
              match Ordering.of_int ([%compare: Term.t] a b) with
              | Equal | Less -> a, b
              | Greater -> b, a
            in
            match Hash_set.mem seen key with
            | true -> ()
            | false ->
              Hash_set.add seen key;
              explain t ~term1:a ~term2:b ~seen ~facts))
  ;;

  (* Builds a lemma that is the negation of [main_literal]'s complement together
     with every asserted fact in [facts]: [main_literal] together with, for each
     fact, the negation of whichever literal is currently asserted (i.e. the
     fact atom negated if it's true, asserted as-is if false). When
     [main_literal] is currently false, this lemma is falsified (a conflict);
     when unassigned, it is unit (a propagation). [add_clause] handles both
     uniformly. *)
  let build_lemma t ~(main_literal : Atom.t * bool) ~(facts : Atom.Hash_set.t) =
    let other_literals =
      Hash_set.to_list facts
      |> List.map ~f:(fun fact_atom ->
        let fact_data = Hashtbl.find_exn t.atoms fact_atom in
        match fact_data.assignment with
        | This true -> fact_atom, false
        | This false -> fact_atom, true
        | Null ->
          (* every fact in an explanation is, by construction, a currently
             asserted atom *)
          assert false)
    in
    `Lemma (main_literal :: other_literals)
  ;;

  (* Theory propagation/conflict detection

     - Conflict: a falsified atom [Eq(a,b)] whose sides have become congruent.
     - Positive propagation: an unassigned atom [Eq(a,b)] whose sides have
       become congruent is forced true.
     - Negative propagation: an unassigned atom [Eq(a,b)] is forced false once
       it's known that its sides must remain distinct, witnessed by some
       falsified atom [Eq(x,y)] with
       [{canonical a, canonical b} = {canonical x, canonical y}] (same-signature
       atoms necessarily share a truth value, so this is sound and, since
       [Eq(x,y)] is false, [a] and [b] cannot merge).

     This is not computed incrementally. *)
  let maybe_get_lemma t =
    let find_conflict () =
      Hash_set.to_list t.falsehoods
      |> List.sort ~compare:Atom.compare
      |> List.find ~f:(fun atom ->
        let (`Eq (a, b)) = atom in
        Ufdsu.same_class
          t.ufdsu
          (canonical_ufds_var t ~term:a)
          (canonical_ufds_var t ~term:b))
    in
    let find_propagation () =
      let false_signatures =
        Hash_set.to_list t.falsehoods
        |> List.sort ~compare:Atom.compare
        |> List.map ~f:(fun atom ->
          let (`Eq (x, y)) = atom in
          atom, (canonical_ufds_var t ~term:x, canonical_ufds_var t ~term:y))
      in
      Hashtbl.data t.atoms
      |> List.sort ~compare:(fun d1 d2 -> Atom.compare d1.atom d2.atom)
      |> List.find_map ~f:(fun atom_data ->
        match atom_data.assignment with
        | This _ -> None
        | Null ->
          let (`Eq (a, b)) = atom_data.atom in
          let ca = canonical_ufds_var t ~term:a in
          let cb = canonical_ufds_var t ~term:b in
          (match ca = cb with
           | true -> Some (`Propagate_true atom_data)
           | false ->
             List.find_map false_signatures ~f:(fun (false_atom, (cx, cy)) ->
               match (ca = cx && cb = cy) || (ca = cy && cb = cx) with
               | true -> Some (`Propagate_false (atom_data, false_atom))
               | false -> None)))
    in
    match find_conflict () with
    | Some atom ->
      let (`Eq (a, b)) = atom in
      let atom_data = Hashtbl.find_exn t.atoms atom in
      let facts = Atom.Hash_set.create () in
      let seen = Term_pair.Hash_set.create () in
      explain t ~term1:a ~term2:b ~seen ~facts;
      build_lemma t ~main_literal:(atom_data.atom, true) ~facts
    | None ->
      (match find_propagation () with
       | None -> `Consistent
       | Some (`Propagate_true atom_data) ->
         let (`Eq (a, b)) = atom_data.atom in
         let facts = Atom.Hash_set.create () in
         let seen = Term_pair.Hash_set.create () in
         explain t ~term1:a ~term2:b ~seen ~facts;
         build_lemma t ~main_literal:(atom_data.atom, true) ~facts
       | Some (`Propagate_false (atom_data, false_atom)) ->
         let (`Eq (a, b)) = atom_data.atom in
         let (`Eq (x, y)) = false_atom in
         let facts = Atom.Hash_set.create () in
         Hash_set.add facts false_atom;
         let seen = Term_pair.Hash_set.create () in
         let ca = canonical_ufds_var t ~term:a in
         let cx = canonical_ufds_var t ~term:x in
         (match ca = cx with
          | true ->
            explain t ~term1:a ~term2:x ~seen ~facts;
            explain t ~term1:b ~term2:y ~seen ~facts
          | false ->
            explain t ~term1:a ~term2:y ~seen ~facts;
            explain t ~term1:b ~term2:x ~seen ~facts);
         build_lemma t ~main_literal:(atom_data.atom, false) ~facts)
  ;;
end
