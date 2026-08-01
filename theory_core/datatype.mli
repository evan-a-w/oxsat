open! Core
open! Feel.Import

(** Metadata and declarations for algebraic datatypes.

    Formula nodes carry constructor, selector, and tester records so terms
    remain self-describing and serializable, but the solver treats a {!Env.t}
    declaration environment as the source of truth. Every ADT constructor,
    selector, and tester used by the solver must be declared. Constructor
    declarations include one field type per constructor argument. Selectors
    project only when applied to values known equal to their owning constructor;
    applications to other constructors are intentionally underspecified. *)

module Datatype : sig
  type t = { name : Tvar.t } [@@deriving sexp, compare, hash, equal]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Constructor : sig
  type t =
    { datatype : Datatype.t
    ; name : Tvar.t
    ; arity : int
    }
  [@@deriving sexp, compare, hash, equal]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Selector : sig
  type t =
    { constructor : Constructor.t
    ; name : Tvar.t
    ; index : int
    }
  [@@deriving sexp, compare, hash, equal]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Constructor_declaration : sig
  type t =
    { constructor : Constructor.t
    ; field_types : Type_expr.t list
    ; selectors : Selector.t list
    }
  [@@deriving sexp, compare, hash, equal]
end

module Declaration : sig
  type t =
    { datatype : Datatype.t
    ; constructors : Constructor_declaration.t list
    }
  [@@deriving sexp, compare, hash, equal]
end

module Env : sig
  type t [@@deriving sexp, compare]

  val empty : t
  val of_declarations : Declaration.t list -> t Or_error.t
  val add : t -> Declaration.t -> t Or_error.t
  val declarations : t -> Declaration.t list
  val find : t -> Datatype.t -> Declaration.t option
  val find_constructor : t -> Constructor.t -> Constructor_declaration.t option
  val mem_constructor : t -> Constructor.t -> bool
  val mem_selector : t -> Selector.t -> bool
  val validate_declaration : Declaration.t -> unit Or_error.t
end
