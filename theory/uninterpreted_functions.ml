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
  module Kind = struct
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

  type t =
    { kind : Kind.t
    ; negated : bool
    ; sat_var : int
    }
  [@@deriving hash, compare, sexp]

  include functor Comparable.Make
  include functor Hashable.Make
end

type t =
  { ufds_var_by_term : int Term.Table.t
  ; ufds_u : Ufdsu.t
  }
