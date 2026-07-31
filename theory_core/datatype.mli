open! Core
open! Feel.Import

(** Metadata carried by algebraic-datatype formula nodes. The solver has no
    separate declaration environment yet; constructor applications, selectors,
    and testers are self-describing through these records.

    [datatype] names the ADT family. Constructors with the same [datatype] and
    different [name] are disjoint, and constructors with the same [datatype],
    [name], and [arity] are injective. A selector is owned by one constructor
    and projects the field at [index] only when applied to a value equal to that
    constructor; applications to other constructors are intentionally
    underspecified. *)

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
