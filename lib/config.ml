open! Core

module Time_bound = struct
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

let default = { emit_trace = false; debug = false }
