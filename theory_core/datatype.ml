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
