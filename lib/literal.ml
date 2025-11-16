open! Core
open! Import
open! Unboxed

type t = I64.t [@@deriving sexp, equal]

let var t = I64.abs t |> I64.to_int_trunc
let value t = I64.O.(t > #0L)
let create ~var ~value = if value then I64.of_int var else I64.of_int (-var)
let compare = I64.compare
let to_int t = I64.to_int_trunc t
let of_int i = I64.of_int i
let negate t = I64.neg t

module Option = I64.Option
