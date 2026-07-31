open! Core
open! Import

module Constructor_term = struct
  type t =
    { constructor : Datatype.Constructor.t
    ; args : Formula.any list
    ; term : Formula.any
    }
end

module Selector_term = struct
  type t =
    { selector : Datatype.Selector.t
    ; argument : Formula.any
    ; term : Formula.any
    }
end

module Tester_term = struct
  type t =
    { constructor : Datatype.Constructor.t
    ; argument : Formula.any
    ; term : Formula.any
    }
end

module Cycle_edge = struct
  type t =
    { source : Formula.any
    ; target : Formula.any
    ; constructor_term : Formula.any
    ; field : Formula.any
    }
  [@@deriving sexp, compare]
end

module Emitted_key = struct
  module T = struct
    type t =
      | Injectivity of Formula.any * Formula.any * int
      | Disjointness of Formula.any * Formula.any
      | Tester of Formula.any * Formula.any * Formula.any
      | Selector of Formula.any * Formula.any
      | Acyclicity of (Formula.any * Formula.any) list
      | Completeness of
          Formula.any * Datatype.Datatype.t * Atom.Equality.t option
    [@@deriving sexp, compare, hash]
  end

  include T
  include functor Hashable.Make
end

module Declared = struct
  type t =
    { declaration : Datatype.Declaration.t
    ; guard : Atom.Equality.t option
    }
  [@@deriving sexp_of]
end

module Scope = struct
  type t =
    { has_adt : bool
    ; env : Datatype.Env.t
    ; declared : Declared.t Datatype.Datatype.Map.t
    ; observations : Datatype.Datatype.Set.t Formula.Any.Map.t
    }
  [@@deriving sexp_of]
end

type t =
  { mutable has_adt : bool
  ; emitted : Emitted_key.Hash_set.t
  ; mutable last_certificate : Proof.Theory_certificate.Adt.t option
  ; mutable env : Datatype.Env.t
  ; mutable declared : Declared.t Datatype.Datatype.Map.t
  ; mutable scopes : Scope.t list
  ; mutable observations : Datatype.Datatype.Set.t Formula.Any.Map.t
  }

let create ?(env = Datatype.Env.empty) () =
  let declared =
    Datatype.Env.declarations env
    |> List.map ~f:(fun declaration ->
      ( declaration.Datatype.Declaration.datatype
      , { Declared.declaration; guard = None } ))
    |> Datatype.Datatype.Map.of_alist_exn
  in
  { has_adt = false
  ; emitted = Emitted_key.Hash_set.create ()
  ; last_certificate = None
  ; env
  ; declared
  ; scopes = []
  ; observations = Formula.Any.Map.empty
  }
;;

let push t =
  t.scopes
  <- { Scope.has_adt = t.has_adt
     ; env = t.env
     ; declared = t.declared
     ; observations = t.observations
     }
     :: t.scopes
;;

let pop t =
  match t.scopes with
  | [] -> assert false
  | scope :: scopes ->
    t.has_adt <- scope.has_adt;
    t.env <- scope.env;
    t.declared <- scope.declared;
    t.observations <- scope.observations;
    t.scopes <- scopes
;;

let declare t ?guard declaration =
  let%map.Or_error env = Datatype.Env.add t.env declaration in
  t.env <- env;
  t.declared
  <- Map.set
       t.declared
       ~key:declaration.datatype
       ~data:{ Declared.declaration; guard }
;;

let env t = t.env

let observe t subject datatype =
  t.observations
  <- Map.update t.observations subject ~f:(function
       | None -> Datatype.Datatype.Set.singleton datatype
       | Some datatypes -> Set.add datatypes datatype)
;;

let rec note_adt_shapes t (term : Formula.any) =
  (match term with
   | Datatype_constructor (constructor, _) ->
     t.has_adt <- true;
     observe t term constructor.datatype
   | Datatype_selector (selector, argument) ->
     t.has_adt <- true;
     observe t argument selector.constructor.datatype
   | Datatype_tester (constructor, argument) ->
     t.has_adt <- true;
     observe t argument constructor.datatype
   | _ -> ());
  List.iter (Formula.args term) ~f:(note_adt_shapes t)
;;

let add_atom t ~atom =
  let left, right = Atom.Equality.endpoints (Atom.Equality.normalize atom) in
  note_adt_shapes t left;
  note_adt_shapes t right;
  match left, right with
  | Datatype_constructor (constructor, _), other
  | other, Datatype_constructor (constructor, _) ->
    observe t other constructor.datatype
  | _ -> ()
;;

let datatype_observations t = t.observations

let rec validate_formula t (formula : Formula.any) =
  let%bind.Or_error () =
    match formula with
    | Datatype_constructor (constructor, args) ->
      if not (Datatype.Env.mem_constructor t.env constructor)
      then
        Or_error.error_s
          [%message
            "undeclared ADT constructor" (constructor : Datatype.Constructor.t)]
      else if List.length args <> constructor.arity
      then
        Or_error.error_s
          [%message
            "ADT constructor application has the wrong arity"
              (constructor : Datatype.Constructor.t)
              ~actual:(List.length args : int)]
      else Ok ()
    | Datatype_selector (selector, _) ->
      if Datatype.Env.mem_selector t.env selector
      then Ok ()
      else
        Or_error.error_s
          [%message "undeclared ADT selector" (selector : Datatype.Selector.t)]
    | Datatype_tester (constructor, _) ->
      if Datatype.Env.mem_constructor t.env constructor
      then Ok ()
      else
        Or_error.error_s
          [%message
            "undeclared ADT tester constructor"
              (constructor : Datatype.Constructor.t)]
    | _ -> Ok ()
  in
  List.fold_result (Formula.args formula) ~init:() ~f:(fun () arg ->
    validate_formula t arg)
;;

let eq left right : Atom.Equality.t = `Eq (left, right)

let tester_term constructor argument =
  Formula.Datatype_tester (constructor, argument)
;;

let tester_atom constructor argument =
  eq (tester_term constructor argument) Formula.True
;;

let register_lemma_atoms egraph literals =
  List.iter literals ~f:(fun (atom, _) ->
    if Option.is_none (Formula_egraph_uf.atom_value egraph ~atom)
    then Formula_egraph_uf.add_atom egraph ~atom)
;;

let canonical egraph term = Formula_egraph_uf.canonical_term egraph ~term

let same_class egraph a b =
  Formula.equal_any (canonical egraph a) (canonical egraph b)
;;

let guarded_equality argument witness =
  if Formula.equal_any argument witness
  then []
  else [ eq argument witness, false ]
;;

let normalized_pair a b = if Formula.compare_any a b <= 0 then a, b else b, a

let constructors terms =
  List.filter_map terms ~f:(function
    | Formula.Datatype_constructor (constructor, args) as term ->
      Some { Constructor_term.constructor; args; term }
    | _ -> None)
;;

let selectors terms =
  List.filter_map terms ~f:(function
    | Formula.Datatype_selector (selector, argument) as term ->
      Some { Selector_term.selector; argument; term }
    | _ -> None)
;;

let testers terms =
  List.filter_map terms ~f:(function
    | Formula.Datatype_tester (constructor, argument) as term ->
      Some { Tester_term.constructor; argument; term }
    | _ -> None)
;;

let emit t egraph key literals certificate =
  if Hash_set.mem t.emitted key
  then None
  else (
    Hash_set.add t.emitted key;
    register_lemma_atoms egraph literals;
    t.last_certificate <- Some certificate;
    Some (`Lemma literals))
;;

let find_disjointness t egraph constructors =
  List.find_mapi constructors ~f:(fun i left ->
    List.drop constructors (i + 1)
    |> List.find_map ~f:(fun right ->
      if Datatype.Datatype.equal
           left.Constructor_term.constructor.datatype
           right.Constructor_term.constructor.datatype
         && (not
               (Datatype.Constructor.equal
                  left.Constructor_term.constructor
                  right.Constructor_term.constructor))
         && same_class
              egraph
              left.Constructor_term.term
              right.Constructor_term.term
      then (
        let a, b =
          normalized_pair left.Constructor_term.term right.Constructor_term.term
        in
        let key = Emitted_key.Disjointness (a, b) in
        let literals =
          [ eq left.Constructor_term.term right.Constructor_term.term, false ]
        in
        let certificate =
          Proof.Theory_certificate.Adt.Disjointness
            { left_constructor = left.Constructor_term.constructor
            ; left_args = left.Constructor_term.args
            ; right_constructor = right.Constructor_term.constructor
            ; right_args = right.Constructor_term.args
            }
        in
        emit t egraph key literals certificate)
      else None))
;;

let find_injectivity t egraph constructors =
  List.find_mapi constructors ~f:(fun i left ->
    List.drop constructors (i + 1)
    |> List.find_map ~f:(fun right ->
      if Datatype.Constructor.equal
           left.Constructor_term.constructor
           right.Constructor_term.constructor
         && same_class
              egraph
              left.Constructor_term.term
              right.Constructor_term.term
      then
        List.find_mapi
          left.Constructor_term.args
          ~f:(fun field_index left_arg ->
            match List.nth right.Constructor_term.args field_index with
            | None -> None
            | Some right_arg ->
              if Formula.equal_any left_arg right_arg
              then None
              else (
                let key =
                  Emitted_key.Injectivity
                    ( left.Constructor_term.term
                    , right.Constructor_term.term
                    , field_index )
                in
                let literals =
                  [ ( eq left.Constructor_term.term right.Constructor_term.term
                    , false )
                  ; eq left_arg right_arg, true
                  ]
                in
                let certificate =
                  Proof.Theory_certificate.Adt.Injectivity
                    { constructor = left.Constructor_term.constructor
                    ; left_args = left.Constructor_term.args
                    ; right_args = right.Constructor_term.args
                    ; field_index
                    }
                in
                emit t egraph key literals certificate))
      else None))
;;

let find_tester t egraph testers constructors =
  List.find_map testers ~f:(fun tester ->
    List.find_map constructors ~f:(fun witness ->
      if Datatype.Datatype.equal
           tester.Tester_term.constructor.datatype
           witness.Constructor_term.constructor.datatype
         && same_class
              egraph
              tester.Tester_term.argument
              witness.Constructor_term.term
      then (
        let value =
          Datatype.Constructor.equal
            tester.Tester_term.constructor
            witness.Constructor_term.constructor
        in
        let key =
          Emitted_key.Tester
            ( tester.Tester_term.term
            , tester.Tester_term.argument
            , witness.Constructor_term.term )
        in
        let literals =
          guarded_equality
            tester.Tester_term.argument
            witness.Constructor_term.term
          @ [ ( tester_atom
                  tester.Tester_term.constructor
                  tester.Tester_term.argument
              , value )
            ]
        in
        let certificate =
          Proof.Theory_certificate.Adt.Tester
            { tester_constructor = tester.Tester_term.constructor
            ; argument = tester.Tester_term.argument
            ; witness_constructor = witness.Constructor_term.constructor
            ; witness_args = witness.Constructor_term.args
            ; value
            }
        in
        emit t egraph key literals certificate)
      else None))
;;

let find_selector t egraph selectors constructors =
  List.find_map selectors ~f:(fun selector ->
    List.find_map constructors ~f:(fun witness ->
      if Datatype.Constructor.equal
           selector.Selector_term.selector.constructor
           witness.Constructor_term.constructor
         && same_class
              egraph
              selector.Selector_term.argument
              witness.Constructor_term.term
      then (
        match
          List.nth
            witness.Constructor_term.args
            selector.Selector_term.selector.index
        with
        | None -> None
        | Some projected ->
          let key =
            Emitted_key.Selector
              (selector.Selector_term.term, witness.Constructor_term.term)
          in
          let literals =
            guarded_equality
              selector.Selector_term.argument
              witness.Constructor_term.term
            @ [ eq selector.Selector_term.term projected, true ]
          in
          let certificate =
            Proof.Theory_certificate.Adt.Selector
              { selector = selector.Selector_term.selector
              ; argument = selector.Selector_term.argument
              ; constructor_args = witness.Constructor_term.args
              }
          in
          emit t egraph key literals certificate)
      else None))
;;

let cycle_key cycle =
  let pairs =
    List.map cycle ~f:(fun edge ->
      edge.Cycle_edge.constructor_term, edge.Cycle_edge.field)
  in
  Emitted_key.Acyclicity pairs
;;

let cycle_literals cycle =
  match cycle with
  | [] -> []
  | _ ->
    let fields = List.map cycle ~f:(fun edge -> edge.Cycle_edge.field) in
    let previous_fields =
      match List.rev fields with
      | [] -> []
      | last :: rev_rest -> last :: List.rev rev_rest
    in
    List.map2_exn cycle previous_fields ~f:(fun edge previous_field ->
      eq edge.Cycle_edge.constructor_term previous_field, false)
;;

let cycle_certificate cycle =
  Proof.Theory_certificate.Adt.Acyclicity
    { cycle =
        List.map cycle ~f:(fun edge ->
          { Proof.Theory_certificate.Adt.Cycle_edge.constructor_term =
              edge.Cycle_edge.constructor_term
          ; field = edge.Cycle_edge.field
          })
    }
;;

let find_cycle edges =
  let by_source = Formula.Any.Table.create () in
  List.iter edges ~f:(fun edge ->
    Hashtbl.add_multi by_source ~key:edge.Cycle_edge.source ~data:edge);
  let rec dfs ~start ~seen path edge =
    if Set.mem seen edge.Cycle_edge.target
    then None
    else if Formula.equal_any edge.Cycle_edge.target start
    then Some (List.rev (edge :: path))
    else (
      let seen = Set.add seen edge.Cycle_edge.target in
      Hashtbl.find_multi by_source edge.Cycle_edge.target
      |> List.find_map ~f:(dfs ~start ~seen (edge :: path)))
  in
  List.find_map edges ~f:(fun edge ->
    dfs ~start:edge.Cycle_edge.source ~seen:Formula.Any.Set.empty [] edge)
;;

let find_acyclicity t egraph constructors =
  let edges =
    List.concat_map constructors ~f:(fun constructor ->
      let source = canonical egraph constructor.Constructor_term.term in
      List.map constructor.Constructor_term.args ~f:(fun field ->
        { Cycle_edge.source
        ; target = canonical egraph field
        ; constructor_term = constructor.Constructor_term.term
        ; field
        }))
  in
  match find_cycle edges with
  | None -> None
  | Some cycle ->
    let key = cycle_key cycle in
    let literals = cycle_literals cycle in
    let certificate = cycle_certificate cycle in
    emit t egraph key literals certificate
;;

let constructor_term constructor = Formula.Datatype_constructor (constructor, [])

let completeness_literals ~guard subject declaration form =
  let guard_literals =
    match guard with
    | None -> []
    | Some atom -> [ atom, false ]
  in
  let constructor_declarations =
    declaration.Datatype.Declaration.constructors
  in
  match (form : Proof.Theory_certificate.Adt.Completeness_form.t) with
  | Enum_equalities ->
    guard_literals
    @ List.map constructor_declarations ~f:(fun cd ->
      eq subject (constructor_term cd.constructor), true)
  | Testers ->
    guard_literals
    @ List.map constructor_declarations ~f:(fun cd ->
      tester_atom cd.constructor subject, true)
;;

let completeness_form declaration =
  if List.for_all declaration.Datatype.Declaration.constructors ~f:(fun cd ->
       cd.Datatype.Constructor_declaration.constructor.arity = 0)
  then Proof.Theory_certificate.Adt.Completeness_form.Enum_equalities
  else Testers
;;

let find_completeness t egraph terms =
  let registered_terms = Formula.Any.Set.of_list terms in
  Map.to_alist t.observations
  |> List.find_map ~f:(fun (subject, datatypes) ->
    match subject with
    | Formula.Datatype_constructor _ -> None
    | _ ->
      if not (Set.mem registered_terms subject)
      then None
      else
        Set.to_list datatypes
        |> List.find_map ~f:(fun datatype ->
          match Map.find t.declared datatype with
          | None -> None
          | Some { Declared.declaration; guard } ->
            let form = completeness_form declaration in
            let key = Emitted_key.Completeness (subject, datatype, guard) in
            let literals =
              completeness_literals ~guard subject declaration form
            in
            let certificate =
              Proof.Theory_certificate.Adt.Completeness
                { declaration; subject; guard; form }
            in
            emit t egraph key literals certificate))
;;

let maybe_get_lemma t ~egraph =
  t.last_certificate <- None;
  if not t.has_adt
  then `Consistent
  else (
    let terms = Formula_egraph_uf.registered_terms egraph in
    let constructors = constructors terms in
    let selectors = selectors terms in
    let testers = testers terms in
    match find_disjointness t egraph constructors with
    | Some lemma -> lemma
    | None ->
      (match find_injectivity t egraph constructors with
       | Some lemma -> lemma
       | None ->
         (match find_tester t egraph testers constructors with
          | Some lemma -> lemma
          | None ->
            (match find_selector t egraph selectors constructors with
             | Some lemma -> lemma
             | None ->
               (match find_acyclicity t egraph constructors with
                | Some lemma -> lemma
                | None ->
                  (match find_completeness t egraph terms with
                   | Some lemma -> lemma
                   | None -> `Consistent))))))
;;

let last_certificate t = t.last_certificate
let undo _ ~to_decision_level_excl:_ = ()
