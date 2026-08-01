open! Core
open! Feel.Import

module Datatype = struct
  module T = struct
    type t = { name : Tvar.t } [@@deriving sexp, compare, hash, equal]
  end

  include T
  include functor Comparable.Make
  include functor Hashable.Make
end

module Constructor = struct
  module T = struct
    type t =
      { datatype : Datatype.t
      ; name : Tvar.t
      ; arity : int
      }
    [@@deriving sexp, compare, hash, equal]
  end

  include T
  include functor Comparable.Make
  include functor Hashable.Make
end

module Selector = struct
  module T = struct
    type t =
      { constructor : Constructor.t
      ; name : Tvar.t
      ; index : int
      }
    [@@deriving sexp, compare, hash, equal]
  end

  include T
  include functor Comparable.Make
  include functor Hashable.Make
end

module Constructor_declaration = struct
  type t =
    { constructor : Constructor.t
    ; field_types : Type_expr.t list
    ; selectors : Selector.t list
    }
  [@@deriving sexp, compare, hash, equal]
end

module Declaration = struct
  type t =
    { datatype : Datatype.t
    ; constructors : Constructor_declaration.t list
    }
  [@@deriving sexp, compare, hash, equal]
end

module Env = struct
  type t =
    { declarations : Declaration.t Datatype.Map.t
    ; constructors : Constructor_declaration.t Constructor.Map.t
    ; selectors : Selector.Set.t
    }
  [@@deriving sexp, compare]

  let empty =
    { declarations = Datatype.Map.empty
    ; constructors = Constructor.Map.empty
    ; selectors = Selector.Set.empty
    }
  ;;

  let validate_constructor declaration_datatype seen_names seen_constructors cd =
    let constructor = cd.Constructor_declaration.constructor in
    if not (Datatype.equal constructor.datatype declaration_datatype)
    then
      Or_error.error_s
        [%message
          "constructor datatype does not match declaration"
            (declaration_datatype : Datatype.t)
            (constructor : Constructor.t)]
    else if Set.mem seen_names constructor.name
    then
      Or_error.error_s
        [%message
          "duplicate constructor name in datatype declaration"
            (constructor.name : Tvar.t)
            (declaration_datatype : Datatype.t)]
    else if Set.mem seen_constructors constructor
    then
      Or_error.error_s
        [%message
          "duplicate constructor in datatype declaration"
            (constructor : Constructor.t)]
    else if List.length cd.field_types <> constructor.arity
    then
      Or_error.error_s
        [%message
          "constructor field type list has the wrong arity"
            (constructor : Constructor.t)
            ~actual:(List.length cd.field_types : int)]
    else (
      let seen_indices = Int.Hash_set.create () in
      let seen_selectors = Selector.Hash_set.create () in
      List.fold_result cd.selectors ~init:() ~f:(fun () selector ->
        if not (Constructor.equal selector.constructor constructor)
        then
          Or_error.error_s
            [%message
              "selector owner does not match constructor declaration"
                (selector : Selector.t)
                (constructor : Constructor.t)]
        else if selector.index < 0 || selector.index >= constructor.arity
        then
          Or_error.error_s
            [%message
              "selector index is outside constructor arity"
                (selector : Selector.t)
                (constructor : Constructor.t)]
        else if Hash_set.mem seen_indices selector.index
        then
          Or_error.error_s
            [%message
              "duplicate selector index in constructor declaration"
                (selector.index : int)
                (constructor : Constructor.t)]
        else if Hash_set.mem seen_selectors selector
        then
          Or_error.error_s
            [%message
              "duplicate selector in constructor declaration"
                (selector : Selector.t)]
        else (
          Hash_set.add seen_indices selector.index;
          Hash_set.add seen_selectors selector;
          Ok ())))
  ;;

  let has_finite_inhabitant (declaration : Declaration.t) =
    let rec field_type_has_inhabitant productive = function
      | Type_expr.App (name, args)
        when Tvar.equal name declaration.datatype.name ->
        productive
        && List.for_all args ~f:(field_type_has_inhabitant productive)
      | App (_, args) ->
        List.for_all args ~f:(field_type_has_inhabitant productive)
      | Function_type (a, b) | Array_type (a, b) ->
        field_type_has_inhabitant productive a
        && field_type_has_inhabitant productive b
      | Var _ | Base _ | Type_of _ | Type -> true
    in
    let constructor_is_productive productive cd =
      List.for_all
        cd.Constructor_declaration.field_types
        ~f:(field_type_has_inhabitant productive)
    in
    let rec fixed_point productive =
      let productive' =
        productive
        || List.exists
             declaration.constructors
             ~f:(constructor_is_productive productive)
      in
      if Bool.equal productive productive'
      then productive
      else fixed_point productive'
    in
    fixed_point false
  ;;

  let validate_declaration declaration =
    if List.is_empty declaration.Declaration.constructors
    then
      Or_error.error_s
        [%message
          "datatype declaration must have at least one constructor"
            (declaration.datatype : Datatype.t)]
    else (
      let%bind.Or_error (_ : Tvar.Set.t * Constructor.Set.t) =
        List.fold_result
          declaration.constructors
          ~init:(Tvar.Set.empty, Constructor.Set.empty)
          ~f:(fun (seen_names, seen_constructors) cd ->
            let%map.Or_error () =
              validate_constructor
                declaration.datatype
                seen_names
                seen_constructors
                cd
            in
            ( Set.add seen_names cd.constructor.name
            , Set.add seen_constructors cd.constructor ))
      in
      if has_finite_inhabitant declaration
      then Ok ()
      else
        Or_error.error_s
          [%message
            "datatype declaration has no finite inhabitant"
              (declaration.datatype : Datatype.t)])
  ;;

  let add t declaration =
    let%bind.Or_error () = validate_declaration declaration in
    if Map.mem t.declarations declaration.datatype
    then
      Or_error.error_s
        [%message
          "datatype is already declared" (declaration.datatype : Datatype.t)]
    else (
      let duplicate_constructor =
        List.find declaration.constructors ~f:(fun cd ->
          Map.mem t.constructors cd.constructor)
      in
      match duplicate_constructor with
      | Some cd ->
        Or_error.error_s
          [%message
            "constructor is already declared" (cd.constructor : Constructor.t)]
      | None ->
        let constructors =
          List.fold
            declaration.constructors
            ~init:t.constructors
            ~f:(fun constructors cd ->
              Map.set constructors ~key:cd.constructor ~data:cd)
        in
        let selectors =
          List.fold
            declaration.constructors
            ~init:t.selectors
            ~f:(fun selectors cd ->
              List.fold cd.selectors ~init:selectors ~f:Set.add)
        in
        Ok
          { declarations =
              Map.set t.declarations ~key:declaration.datatype ~data:declaration
          ; constructors
          ; selectors
          })
  ;;

  let of_declarations declarations =
    List.fold_result declarations ~init:empty ~f:add
  ;;

  let declarations t = Map.data t.declarations
  let find t datatype = Map.find t.declarations datatype
  let find_constructor t constructor = Map.find t.constructors constructor
  let mem_constructor t constructor = Map.mem t.constructors constructor
  let mem_selector t selector = Set.mem t.selectors selector
end
