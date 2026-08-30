open! Core
open! Feel.Import

module Bounds : sig
  type t =
    { lower : Q.t
    ; upper : Q.t
    }
  [@@deriving sexp, compare, hash]
end

type t =
  { integral : bool
  ; bounds : Bounds.t option
  }
[@@deriving sexp, compare, hash]

val of_base : Type_expr.Base.t -> t option
val of_type_expr : Type_expr.t -> t option
val int64_bounds : Bounds.t
val is_numeric_base : Type_expr.Base.t -> bool
val is_integral_base : Type_expr.Base.t -> bool
