open! Core
open! Feel.Import

module Bounds = struct
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

let int64_bounds =
  { Bounds.lower = Q.of_int64 Int64.min_value
  ; upper = Q.of_int64 Int64.max_value
  }
;;

let of_base = function
  | Type_expr.Base.Bool -> None
  | Real -> Some { integral = false; bounds = None }
  | Int -> Some { integral = true; bounds = None }
  | Int64 -> Some { integral = true; bounds = Some int64_bounds }
;;

let of_type_expr = function
  | Type_expr.Base base -> of_base base
  | Var _ | Type_of _ | App _ | Function_type _ | Array_type _ | Type -> None
;;

let is_numeric_base base = Option.is_some (of_base base)

let is_integral_base base =
  Option.value_map (of_base base) ~default:false ~f:(fun t -> t.integral)
;;
