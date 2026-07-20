open! Core
open! Import
module Atom = Import.Atom.Equality
module G = Formula_egraph.Graph

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
        { op : Formula.Op.t
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
  { id_by_term : G.Id.t Formula.Any.Table.t
  ; term_by_id : Formula.any G.Id.Table.t
  ; node_by_id : G.Node.t G.Id.Table.t
  ; egraph : G.t
  ; atoms : Atom_data.t Atom.Table.t
  ; (* Terms (including subterms) that occur in some registered atom, as opposed
       to terms merely registered via [add_term] for e-matching. Only these can
       influence an atom's truth, so only their arrangement matters to EUF. *)
    atom_terms : Formula.Any.Hash_set.t
  ; explanation_forest : Explanation_forest.t
  ; trail : Trail_entry.t list ref
  ; falsehoods : Atom.Hash_set.t
  ; current_decision_level : int ref
  ; mutable last_certificate : Lemma_certificate.Euf.t option
  }

let add_explanation_edge t ~x ~y ~justification ~decision_level =
  Explanation_forest.add_edge t.explanation_forest ~x ~y ~justification;
  t.trail := Trail_entry.Explanation_edge { x; y; decision_level } :: !(t.trail)
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

let rec register_term t ~(term : Formula.any) : G.Id.t =
  match Hashtbl.find t.id_by_term term with
  | Some id -> id
  | None ->
    let op = Formula.op term in
    let children =
      List.map (Formula.args term) ~f:(fun arg -> register_term t ~term:arg)
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
       let on_merge ~winner ~loser =
         let n1 = Hashtbl.find_exn t.node_by_id winner in
         let n2 = Hashtbl.find_exn t.node_by_id loser in
         add_explanation_edge
           t
           ~x:(G.Id.to_int winner)
           ~y:(G.Id.to_int loser)
           ~justification:
             (Justification.Congruence
                { op = n1.op; args1 = n1.children; args2 = n2.children })
           ~decision_level:dl
       in
       G.set_decision_level t.egraph dl;
       G.merge t.egraph ~on_merge id existing_id;
       G.rebuild t.egraph ~on_merge ());
    id
;;

let rec mark_atom_term t (term : Formula.any) =
  if not (Hash_set.mem t.atom_terms term)
  then (
    Hash_set.add t.atom_terms term;
    List.iter (Formula.args term) ~f:(mark_atom_term t))
;;

let register_atom t ~(atom : Atom.t) =
  let atom = Atom.normalize atom in
  let a, b = Atom.endpoints atom in
  let id_a = register_term t ~term:a in
  let id_b = register_term t ~term:b in
  mark_atom_term t a;
  mark_atom_term t b;
  Hashtbl.set t.atoms ~key:atom ~data:{ atom; id_a; id_b; assignment = Null }
;;

let create ~atoms =
  let t =
    { id_by_term = Formula.Any.Table.create ()
    ; term_by_id = G.Id.Table.create ()
    ; node_by_id = G.Id.Table.create ()
    ; egraph = G.create ()
    ; atoms = Atom.Table.create ()
    ; atom_terms = Formula.Any.Hash_set.create ()
    ; explanation_forest = Explanation_forest.create ()
    ; trail = ref []
    ; falsehoods = Atom.Hash_set.create ()
    ; current_decision_level = ref 0
    ; last_certificate = None
    }
  in
  List.iter atoms ~f:(fun atom -> register_atom t ~atom);
  t
;;

let add_atom t ~atom = register_atom t ~atom
let add_term t ~term = ignore (register_term t ~term : G.Id.t)
let mem_term t term = Hashtbl.mem t.id_by_term term
let mem_atom_term t term = Hash_set.mem t.atom_terms term

let canonical_term t ~term =
  let id = Hashtbl.find_exn t.id_by_term term in
  let cid = G.canonical t.egraph id in
  Hashtbl.find_exn t.term_by_id cid
;;

let registered_terms t = Hashtbl.keys t.id_by_term
let egraph t = t.egraph
let term_of_id t id = Hashtbl.find t.term_by_id id

let classes t =
  Hashtbl.keys t.id_by_term
  |> List.map ~f:(fun term -> term, canonical_term t ~term)
;;

let assert_true_atom t ~decision_level ~(atom : Atom.t) =
  match Hashtbl.find_or_null t.atoms atom with
  | Null -> ()
  | This atom_data ->
    t.current_decision_level := decision_level;
    atom_data.assignment <- This true;
    t.trail := Trail_entry.Truth { atom; decision_level } :: !(t.trail);
    let id_a = atom_data.id_a in
    let id_b = atom_data.id_b in
    let first_merge = ref true in
    let on_merge ~winner ~loser =
      match !first_merge with
      | true ->
        (* The direct merge this assertion requested. [winner]/[loser] are the
           union-find's post-canonicalization ids, which needn't be [id_a]/
           [id_b] (e.g. one of them may already have been absorbed into another
           class by an earlier, unrelated merge) -- recording the edge between
           the *union-find's* choice of ids would silently skip whatever fact
           connected [id_a] or [id_b] to their current class, breaking
           [explain]'s ability to reconstruct the full justification chain.
           Recording it between [id_a]/[id_b] directly keeps every asserted fact
           reachable regardless of union-by-rank tie-breaking. *)
        first_merge := false;
        add_explanation_edge
          t
          ~x:(G.Id.to_int id_a)
          ~y:(G.Id.to_int id_b)
          ~justification:(Justification.Asserted atom)
          ~decision_level
      | false ->
        let n1 = Hashtbl.find_exn t.node_by_id winner in
        let n2 = Hashtbl.find_exn t.node_by_id loser in
        add_explanation_edge
          t
          ~x:(G.Id.to_int winner)
          ~y:(G.Id.to_int loser)
          ~justification:
            (Justification.Congruence
               { op = n1.op; args1 = n1.children; args2 = n2.children })
          ~decision_level
    in
    G.set_decision_level t.egraph decision_level;
    G.merge t.egraph ~on_merge id_a id_b;
    G.rebuild t.egraph ~on_merge ()
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

let atom_value t ~(atom : Atom.t) =
  let atom = Atom.normalize atom in
  match Hashtbl.find t.atoms atom with
  | None -> None
  | Some atom_data ->
    (match atom_data.assignment with
     | Null -> None
     | This value -> Some value)
;;

let explanation_path t ~from ~to_ =
  match from = to_ with
  | true -> []
  | false ->
    let visited = Int.Hash_set.create () in
    let came_from : (int * Justification.t) Int.Table.t = Int.Table.create () in
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
      | Justification.Congruence { op = _; args1; args2 } ->
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

(* Like [explanation_path], but keeps each edge's endpoint ids so a structured
   proof can name the terms an [Asserted]/[Congruence] step equates. *)
let explanation_path_with_endpoints t ~from ~to_ =
  match from = to_ with
  | true -> []
  | false ->
    let visited = Int.Hash_set.create () in
    let came_from : (int * Justification.t) Int.Table.t = Int.Table.create () in
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
        build ((prev, node, justification) :: acc) prev
    in
    build [] to_
;;

module Certificate = struct
  module Pc = Proof.Theory_certificate.Euf
  module Lc = Lemma_certificate.Euf

  (* [term_by_id] is keyed by [G.Id.t], but the explanation forest works in raw
     [int] node ids; build the [int -> term] view on demand (proof production is
     off the hot path). *)
  let term_by_int t =
    let table = Int.Table.create () in
    Hashtbl.iteri t.term_by_id ~f:(fun ~key ~data ->
      Hashtbl.set table ~key:(G.Id.to_int key) ~data);
    table
  ;;

  let equality a b : Pc.Equality.t = { left = a; right = b }

  (* Flattened proof that [term_of_id from = term_of_id to_]. The checker
     ([check_equality_proof]) requires each congruence step's argument
     equalities to be established by justifications appearing earlier in the
     path, so a congruence edge's sub-proofs are spliced in before the
     congruence justification itself. [seen] deduplicates argument-pair
     sub-proofs, mirroring the flat [explain]. *)
  let equality_proof t ~term_of_id ~from ~to_ : Lc.Equality_proof.t =
    let seen = Id_pair.Hash_set.create () in
    let justifications = ref [] in
    let emit j = justifications := j :: !justifications in
    let rec go ~from ~to_ =
      let path = explanation_path_with_endpoints t ~from ~to_ in
      List.iter path ~f:(fun (x, y, justification) ->
        match justification with
        | Justification.Asserted atom ->
          emit
            (Lc.Justification.Asserted { disequality = (atom :> Import.Atom.t) })
        | Justification.Congruence _ ->
          (* Derive the argument pairs from the edge's actual endpoint terms
             (rather than the justification's stored winner/loser children),
             since the path may traverse the edge in either direction. The
             checker requires [argument_equalities] to be exactly
             [zip (args left) (args right)]. *)
          let left = term_of_id x in
          let right = term_of_id y in
          let left_args = Formula.args left in
          let right_args = Formula.args right in
          List.iter2_exn left_args right_args ~f:(fun a b ->
            let ida = Hashtbl.find_exn t.id_by_term a in
            let idb = Hashtbl.find_exn t.id_by_term b in
            let key : Id_pair.t =
              if G.Id.to_int ida <= G.Id.to_int idb then ida, idb else idb, ida
            in
            if not (Hash_set.mem seen key)
            then (
              Hash_set.add seen key;
              go ~from:(G.Id.to_int ida) ~to_:(G.Id.to_int idb)));
          let argument_equalities =
            List.map2_exn left_args right_args ~f:equality
          in
          emit
            (Lc.Justification.Congruence { left; right; argument_equalities }))
    in
    go ~from ~to_;
    { conclusion = equality (term_of_id from) (term_of_id to_)
    ; path = List.rev !justifications
    }
  ;;

  let of_conflict t ~(main_atom_data : Atom_data.t) : Lc.t =
    let terms = term_by_int t in
    let term_of_id = Hashtbl.find_exn terms in
    Equality
      (equality_proof
         t
         ~term_of_id
         ~from:(G.Id.to_int main_atom_data.id_a)
         ~to_:(G.Id.to_int main_atom_data.id_b))
  ;;

  let of_disequality
    t
    ~(main_atom_data : Atom_data.t)
    ~(false_data : Atom_data.t)
    : Lc.t
    =
    let terms = term_by_int t in
    let term_of_id = Hashtbl.find_exn terms in
    let proof ~from ~to_ = equality_proof t ~term_of_id ~from ~to_ in
    let ca = G.canonical t.egraph main_atom_data.id_a in
    let cx = G.canonical t.egraph false_data.id_a in
    let left_path, right_path =
      match G.Id.equal ca cx with
      | true ->
        ( proof
            ~from:(G.Id.to_int main_atom_data.id_a)
            ~to_:(G.Id.to_int false_data.id_a)
        , proof
            ~from:(G.Id.to_int main_atom_data.id_b)
            ~to_:(G.Id.to_int false_data.id_b) )
      | false ->
        ( proof
            ~from:(G.Id.to_int main_atom_data.id_a)
            ~to_:(G.Id.to_int false_data.id_b)
        , proof
            ~from:(G.Id.to_int main_atom_data.id_b)
            ~to_:(G.Id.to_int false_data.id_a) )
    in
    let a, b = Import.Atom.Equality.endpoints main_atom_data.atom in
    Disequality
      { conclusion = equality a b
      ; asserted_disequality = (false_data.atom :> Import.Atom.t)
      ; left_path
      ; right_path
      }
  ;;
end

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
        (atom, (G.canonical t.egraph data.id_a, G.canonical t.egraph data.id_b))
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
  t.last_certificate <- None;
  match find_conflict () with
  | Some atom ->
    let atom_data = Hashtbl.find_exn t.atoms atom in
    let facts = Atom.Hash_set.create () in
    let seen = Id_pair.Hash_set.create () in
    explain t ~id1:atom_data.id_a ~id2:atom_data.id_b ~seen ~facts;
    t.last_certificate
    <- Some (Certificate.of_conflict t ~main_atom_data:atom_data);
    build_lemma t ~main_literal:(atom_data.atom, true) ~facts
  | None ->
    (match find_propagation () with
     | None -> `Consistent
     | Some (`Propagate_true atom_data) ->
       let facts = Atom.Hash_set.create () in
       let seen = Id_pair.Hash_set.create () in
       explain t ~id1:atom_data.id_a ~id2:atom_data.id_b ~seen ~facts;
       t.last_certificate
       <- Some (Certificate.of_conflict t ~main_atom_data:atom_data);
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
       t.last_certificate
       <- Some
            (Certificate.of_disequality t ~main_atom_data:atom_data ~false_data);
       build_lemma t ~main_literal:(atom_data.atom, false) ~facts)
;;

let last_certificate t = t.last_certificate
