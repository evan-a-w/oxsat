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

  module Atom_data = struct
    type t =
      { atom : Atom.t
      ; id_a : G.Id.t
      ; id_b : G.Id.t
      ; mutable assignment : bool or_null [@compare.ignore]
      }
    [@@deriving hash, compare, sexp]
  end

  module Justification = struct
    type t =
      | Asserted of Atom.t
      | Congruence of
          { function_ : Tvar.t
          ; args1 : G.Id.t list
          ; args2 : G.Id.t list
          }
    [@@deriving sexp_of]
  end

  module Id_pair = struct
    type t = G.Id.t * G.Id.t [@@deriving sexp, compare, hash]

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
    ; node_by_id : G.Node.t G.Id.Table.t
    ; egraph : G.t
    ; atoms : Atom_data.t Atom.Table.t
    ; explanation_forest : Explanation_forest.t
    ; trail : Trail_entry.t list ref
    ; falsehoods : Atom.Hash_set.t
    ; current_decision_level : int ref
    }

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

  let rec register_term t ~(term : Term.t) : G.Id.t =
    match Hashtbl.find t.id_by_term term with
    | Some id -> id
    | None ->
      let op, children =
        match Term.split_function term with
        | None -> Op.Var term, []
        | Some (function_, args) ->
          let children =
            List.map args ~f:(fun arg -> register_term t ~term:arg)
          in
          Op.App function_, children
      in
      let node = { G.Node.op; children } in
      let id = G.add_fresh t.egraph node in
      Hashtbl.set t.id_by_term ~key:term ~data:id;
      Hashtbl.set t.term_by_id ~key:id ~data:term;
      Hashtbl.set t.node_by_id ~key:id ~data:node;
      (match G.lookup t.egraph node with
       | None -> assert false
       | Some existing_id when G.Id.equal existing_id id -> ()
       | Some existing_id ->
         let dl = !(t.current_decision_level) in
         G.set_on_merge t.egraph (fun ~winner ~loser ->
           let n1 = Hashtbl.find_exn t.node_by_id winner in
           let n2 = Hashtbl.find_exn t.node_by_id loser in
           match n1.op, n2.op with
           | Op.App function_, Op.App _ ->
             add_explanation_edge
               t
               ~x:(G.Id.to_int winner)
               ~y:(G.Id.to_int loser)
               ~justification:
                 (Justification.Congruence
                    { function_; args1 = n1.children; args2 = n2.children })
               ~decision_level:dl
           | _ -> assert false);
         G.set_decision_level t.egraph dl;
         G.merge t.egraph id existing_id;
         G.rebuild t.egraph;
         G.set_on_merge t.egraph (fun ~winner:_ ~loser:_ -> ()));
      id
  ;;

  let register_atom t ~(atom : Atom.t) =
    let atom = Atom.normalize atom in
    let (`Eq (a, b)) = atom in
    let id_a = register_term t ~term:a in
    let id_b = register_term t ~term:b in
    Hashtbl.set t.atoms ~key:atom ~data:{ atom; id_a; id_b; assignment = Null }
  ;;

  let create ~atoms =
    let t =
      { id_by_term = Term.Table.create ()
      ; term_by_id = G.Id.Table.create ()
      ; node_by_id = G.Id.Table.create ()
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
    let id = Hashtbl.find_exn t.id_by_term term in
    let cid = G.canonical t.egraph id in
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
      let id_a = atom_data.id_a in
      let id_b = atom_data.id_b in
      let first_merge = ref (Some atom) in
      G.set_on_merge t.egraph (fun ~winner ~loser ->
        let justification =
          match !first_merge with
          | Some asserted_atom ->
            first_merge := None;
            Justification.Asserted asserted_atom
          | None ->
            let n1 = Hashtbl.find_exn t.node_by_id winner in
            let n2 = Hashtbl.find_exn t.node_by_id loser in
            (match n1.op, n2.op with
             | Op.App function_, Op.App _ ->
               Justification.Congruence
                 { function_; args1 = n1.children; args2 = n2.children }
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

  let rec explain t ~(id1 : G.Id.t) ~(id2 : G.Id.t) ~seen ~facts =
    let v1 = G.Id.to_int id1 in
    let v2 = G.Id.to_int id2 in
    match v1 = v2 with
    | true -> ()
    | false ->
      let path = explanation_path t ~from:v1 ~to_:v2 in
      List.iter path ~f:(function
        | Justification.Asserted atom -> Hash_set.add facts atom
        | Justification.Congruence { function_ = _; args1; args2 } ->
          List.iter2_exn args1 args2 ~f:(fun a b ->
            let key : Id_pair.t =
              if G.Id.to_int a <= G.Id.to_int b then a, b else b, a
            in
            match Hash_set.mem seen key with
            | true -> ()
            | false ->
              Hash_set.add seen key;
              explain t ~id1:a ~id2:b ~seen ~facts))
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
      Hash_set.fold t.falsehoods ~init:None ~f:(fun acc atom ->
        match acc with
        | Some _ -> acc
        | None ->
          let data = Hashtbl.find_exn t.atoms atom in
          if G.equivalent t.egraph data.id_a data.id_b then Some atom else None)
    in
    let find_propagation () =
      let false_sigs =
        Hash_set.fold t.falsehoods ~init:[] ~f:(fun acc atom ->
          let data = Hashtbl.find_exn t.atoms atom in
          ( atom
          , (G.canonical t.egraph data.id_a, G.canonical t.egraph data.id_b) )
          :: acc)
      in
      Hashtbl.fold t.atoms ~init:None ~f:(fun ~key:_ ~data:atom_data acc ->
        match acc with
        | Some _ -> acc
        | None ->
          (match atom_data.assignment with
           | This _ -> None
           | Null ->
             let ca = G.canonical t.egraph atom_data.id_a in
             let cb = G.canonical t.egraph atom_data.id_b in
             (match G.Id.equal ca cb with
              | true -> Some (`Propagate_true atom_data)
              | false ->
                List.find_map false_sigs ~f:(fun (false_atom, (cx, cy)) ->
                  if (G.Id.equal ca cx && G.Id.equal cb cy)
                     || (G.Id.equal ca cy && G.Id.equal cb cx)
                  then Some (`Propagate_false (atom_data, false_atom))
                  else None))))
    in
    match find_conflict () with
    | Some atom ->
      let atom_data = Hashtbl.find_exn t.atoms atom in
      let facts = Atom.Hash_set.create () in
      let seen = Id_pair.Hash_set.create () in
      explain t ~id1:atom_data.id_a ~id2:atom_data.id_b ~seen ~facts;
      build_lemma t ~main_literal:(atom_data.atom, true) ~facts
    | None ->
      (match find_propagation () with
       | None -> `Consistent
       | Some (`Propagate_true atom_data) ->
         let facts = Atom.Hash_set.create () in
         let seen = Id_pair.Hash_set.create () in
         explain t ~id1:atom_data.id_a ~id2:atom_data.id_b ~seen ~facts;
         build_lemma t ~main_literal:(atom_data.atom, true) ~facts
       | Some (`Propagate_false (atom_data, false_atom)) ->
         let facts = Atom.Hash_set.create () in
         Hash_set.add facts false_atom;
         let seen = Id_pair.Hash_set.create () in
         let false_data = Hashtbl.find_exn t.atoms false_atom in
         let ca = G.canonical t.egraph atom_data.id_a in
         let cx = G.canonical t.egraph false_data.id_a in
         (match G.Id.equal ca cx with
          | true ->
            explain t ~id1:atom_data.id_a ~id2:false_data.id_a ~seen ~facts;
            explain t ~id1:atom_data.id_b ~id2:false_data.id_b ~seen ~facts
          | false ->
            explain t ~id1:atom_data.id_a ~id2:false_data.id_b ~seen ~facts;
            explain t ~id1:atom_data.id_b ~id2:false_data.id_a ~seen ~facts);
         build_lemma t ~main_literal:(atom_data.atom, false) ~facts)
  ;;
end
