open! Core
open! Feel.Import

module Term = struct
  type t =
    [ `App of function_:Tvar.t * arg:Tvar.t
    | `Var of Tvar.t
    ]
  [@@deriving sexp, compare, hash]

  include functor Comparable.Make
  include functor Hashable.Make
end

module Atom = struct
  type t = [ `Eq of Term.t * Term.t ] [@@deriving sexp, compare, hash]

  include functor Comparable.Make
  include functor Hashable.Make
end
