open! Core
open! Feel.Import
include Uninterpreted_functions_intf

module Make (Term : Term) = struct
  module Term = Term

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
      ; mutable assignment : bool or_null [@compare.ignore]
      }
    [@@deriving hash, compare, sexp]
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

  module Op = struct
    type t =
      | Var of Term.t
      | App of Tvar.t
    [@@deriving compare, sexp, hash]

    include functor Comparable.Make
    include functor Hashable.Make
  end

  module G = Egx.Graph.Make (struct
      module Op = Op

      module Class_metadata = struct
        type t = unit [@@deriving compare, sexp]

        module Query = struct
          type t = unit [@@deriving compare, sexp, hash]
        end

        module Binary_query = struct
          type t = unit [@@deriving compare, sexp, hash]
        end

        let matches () ~query:() = true
        let matches_binary () () ~query:() = true
        let empty = ()
        let add () ~op:_ ~args:_ = ()
        let remove () ~op:_ ~args:_ = ()
      end
    end)

  module Trail_entry = struct
    type t =
      | Truth of
          { atom : Atom.t
          ; decision_level : int
          }
      | Falsehood of
          { atom : Atom.t
          ; decision_level : int
          }
      | Explanation_edge of
          { x : int
          ; y : int
          ; decision_level : int
          }

    let decision_level = function
      | Truth { decision_level; _ }
      | Falsehood { decision_level; _ }
      | Explanation_edge { decision_level; _ } -> decision_level
    ;;
  end

  type t =
    { id_by_term : G.Id.t Term.Table.t
    ; term_by_id : Term.t G.Id.Table.t
    ; egraph : G.t
    ; atoms : Atom_data.t Atom.Table.t
    ; explanation_forest : Explanation_forest.t
    ; trail : Trail_entry.t list ref
    ; falsehoods : Atom.Hash_set.t
    ; current_decision_level : int ref
    }

  let canonical_id t ~term =
    G.canonical t.egraph (Hashtbl.find_exn t.id_by_term term)
  ;;

  let add_explanation_edge t ~x ~y ~justification ~decision_level =
    Explanation_forest.add_edge t.explanation_forest ~x ~y ~justification;
    t.trail
    := Trail_entry.Explanation_edge { x; y; decision_level } :: !(t.trail)
  ;;

  let rec undo t ~to_decision_level_excl =
    G.undo t.egraph ~to_decision_level_excl;
    match !(t.trail) with
    | [] -> ()
    | entry :: rest ->
      if Trail_entry.decision_level entry <= to_decision_level_excl
      then ()
      else (
        t.trail := rest;
        (match entry with
         | Trail_entry.Falsehood { atom; _ } ->
           Hash_set.remove t.falsehoods atom;
           (Hashtbl.find_exn t.atoms atom).assignment <- Null
         | Trail_entry.Truth { atom; _ } ->
           (Hashtbl.find_exn t.atoms atom).assignment <- Null
         | Trail_entry.Explanation_edge { x; y; _ } ->
           Explanation_forest.remove_edge t.explanation_forest ~x ~y);
        undo t ~to_decision_level_excl)
  ;;

  let rec register_term t ~(term : Term.t) =
    match Hashtbl.find t.id_by_term term with
    | Some _ -> ()
    | None ->
      let op, children =
        match Term.split_function term with
        | None -> Op.Var term, []
        | Some (function_, args) ->
          List.iter args ~f:(fun arg -> register_term t ~term:arg);
          let children =
            List.map args ~f:(fun arg -> Hashtbl.find_exn t.id_by_term arg)
          in
          Op.App function_, children
      in
      let node = { G.Node.op; children } in
      let id = G.add_fresh t.egraph node in
      Hashtbl.set t.id_by_term ~key:term ~data:id;
      Hashtbl.set t.term_by_id ~key:id ~data:term;
      (* If a congruent class already exists (because children are merged),
         merge with it so equivalence queries stay correct and explanation edges
         are recorded. *)
      (match G.lookup t.egraph node with
       | None -> assert false
       | Some existing_id when G.Id.equal existing_id id -> ()
       | Some existing_id ->
         let dl = !(t.current_decision_level) in
         G.set_on_merge t.egraph (fun ~winner ~loser ->
           let t1 = Hashtbl.find_exn t.term_by_id winner in
           let t2 = Hashtbl.find_exn t.term_by_id loser in
           match Term.split_function t1, Term.split_function t2 with
           | Some (function_, args1), Some (_, args2) ->
             add_explanation_edge
               t
               ~x:(G.Id.to_int winner)
               ~y:(G.Id.to_int loser)
               ~justification:
                 (Justification.Congruence { function_; args1; args2 })
               ~decision_level:dl
           | _ -> assert false);
         G.set_decision_level t.egraph dl;
         G.merge t.egraph id existing_id;
         G.rebuild t.egraph;
         G.set_on_merge t.egraph (fun ~winner:_ ~loser:_ -> ()))
  ;;

  let register_atom t ~(atom : Atom.t) =
    let atom = Atom.normalize atom in
    let (`Eq (a, b)) = atom in
    register_term t ~term:a;
    register_term t ~term:b;
    Hashtbl.set t.atoms ~key:atom ~data:{ atom; assignment = Null }
  ;;

  let create ~atoms =
    let t =
      { id_by_term = Term.Table.create ()
      ; term_by_id = G.Id.Table.create ()
      ; egraph = G.create ()
      ; atoms = Atom.Table.create ()
      ; explanation_forest = Explanation_forest.create ()
      ; trail = ref []
      ; falsehoods = Atom.Hash_set.create ()
      ; current_decision_level = ref 0
      }
    in
    List.iter atoms ~f:(fun atom -> register_atom t ~atom);
    t
  ;;

  let add_atom t ~atom = register_atom t ~atom

  let canonical_term t ~term =
    let cid = canonical_id t ~term in
    Hashtbl.find_exn t.term_by_id cid
  ;;

  let registered_terms t = Hashtbl.keys t.id_by_term

  let assert_true_atom t ~decision_level ~(atom : Atom.t) =
    match Hashtbl.find_or_null t.atoms atom with
    | Null -> ()
    | This atom_data ->
      t.current_decision_level := decision_level;
      atom_data.assignment <- This true;
      t.trail := Trail_entry.Truth { atom; decision_level } :: !(t.trail);
      let (`Eq (a, b)) = atom in
      let id_a = Hashtbl.find_exn t.id_by_term a in
      let id_b = Hashtbl.find_exn t.id_by_term b in
      (* Use a ref to distinguish the direct (asserted) merge from congruence
         merges triggered by rebuild. The first callback invocation consumes the
         ref; subsequent ones (during rebuild) reconstruct a Congruence
         justification from the merged classes. *)
      let first_merge = ref (Some atom) in
      G.set_on_merge t.egraph (fun ~winner ~loser ->
        let justification =
          match !first_merge with
          | Some asserted_atom ->
            first_merge := None;
            Justification.Asserted asserted_atom
          | None ->
            let t1 = Hashtbl.find_exn t.term_by_id winner in
            let t2 = Hashtbl.find_exn t.term_by_id loser in
            (match Term.split_function t1, Term.split_function t2 with
             | Some (function_, args1), Some (_, args2) ->
               Justification.Congruence { function_; args1; args2 }
             | _ -> assert false)
        in
        add_explanation_edge
          t
          ~x:(G.Id.to_int winner)
          ~y:(G.Id.to_int loser)
          ~justification
          ~decision_level);
      G.set_decision_level t.egraph decision_level;
      G.merge t.egraph id_a id_b;
      G.rebuild t.egraph;
      G.set_on_merge t.egraph (fun ~winner:_ ~loser:_ -> ())
  ;;

  let assert_false_atom t ~decision_level ~(atom : Atom.t) =
    match Hashtbl.find_or_null t.atoms atom with
    | Null -> ()
    | This atom_data ->
      t.current_decision_level := decision_level;
      (match Hash_set.mem t.falsehoods atom with
       | true -> ()
       | false ->
         Hash_set.add t.falsehoods atom;
         atom_data.assignment <- This false;
         t.trail := Trail_entry.Falsehood { atom; decision_level } :: !(t.trail))
  ;;

  let assert_atom t ~decision_level ~(atom : Atom.t) ~value =
    let atom = Atom.normalize atom in
    match value with
    | true -> assert_true_atom t ~decision_level ~atom
    | false -> assert_false_atom t ~decision_level ~atom
  ;;

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

  let rec explain t ~term1 ~term2 ~seen ~facts =
    let v1 = G.Id.to_int (Hashtbl.find_exn t.id_by_term term1) in
    let v2 = G.Id.to_int (Hashtbl.find_exn t.id_by_term term2) in
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

  let build_lemma t ~(main_literal : Atom.t * bool) ~(facts : Atom.Hash_set.t) =
    let other_literals =
      Hash_set.to_list facts
      |> List.map ~f:(fun fact_atom ->
        let fact_data = Hashtbl.find_exn t.atoms fact_atom in
        match fact_data.assignment with
        | This true -> fact_atom, false
        | This false -> fact_atom, true
        | Null -> assert false)
    in
    `Lemma (main_literal :: other_literals)
  ;;

  let maybe_get_lemma t =
    let find_conflict () =
      Hash_set.to_list t.falsehoods
      |> List.sort ~compare:Atom.compare
      |> List.find ~f:(fun atom ->
        let (`Eq (a, b)) = atom in
        G.equivalent
          t.egraph
          (Hashtbl.find_exn t.id_by_term a)
          (Hashtbl.find_exn t.id_by_term b))
    in
    let find_propagation () =
      let false_signatures =
        Hash_set.to_list t.falsehoods
        |> List.sort ~compare:Atom.compare
        |> List.map ~f:(fun atom ->
          let (`Eq (x, y)) = atom in
          atom, (canonical_id t ~term:x, canonical_id t ~term:y))
      in
      Hashtbl.data t.atoms
      |> List.sort ~compare:(fun d1 d2 -> Atom.compare d1.atom d2.atom)
      |> List.find_map ~f:(fun atom_data ->
        match atom_data.assignment with
        | This _ -> None
        | Null ->
          let (`Eq (a, b)) = atom_data.atom in
          let ca = canonical_id t ~term:a in
          let cb = canonical_id t ~term:b in
          (match G.Id.equal ca cb with
           | true -> Some (`Propagate_true atom_data)
           | false ->
             List.find_map false_signatures ~f:(fun (false_atom, (cx, cy)) ->
               match
                 (G.Id.equal ca cx && G.Id.equal cb cy)
                 || (G.Id.equal ca cy && G.Id.equal cb cx)
               with
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
         let ca = canonical_id t ~term:a in
         let cx = canonical_id t ~term:x in
         (match G.Id.equal ca cx with
          | true ->
            explain t ~term1:a ~term2:x ~seen ~facts;
            explain t ~term1:b ~term2:y ~seen ~facts
          | false ->
            explain t ~term1:a ~term2:y ~seen ~facts;
            explain t ~term1:b ~term2:x ~seen ~facts);
         build_lemma t ~main_literal:(atom_data.atom, false) ~facts)
  ;;
end
