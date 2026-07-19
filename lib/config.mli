open! Core

module Time_bound : sig
  type t =
    [ `Unlimited
    | `Bounded of int
    ]
  [@@deriving sexp, compare]
end

type t =
  { emit_trace : bool
  ; debug : bool
  }
[@@deriving sexp, compare]

val default : t
