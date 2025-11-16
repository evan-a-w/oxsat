open! Core
open! Import

let rec luby_term n =
  let rec find_k k =
    if I64.O.((I64.shift_left #1L (I64.to_int_trunc k)) - #1L < n)
    then find_k I64.O.(k + #1L)
    else k
  in
  let k = find_k #1L in
  if I64.O.(n = (I64.shift_left #1L (I64.to_int_trunc k)) - #1L)
  then I64.shift_left #1L (I64.to_int_trunc I64.O.(k - #1L))
  else (
    let prev_block = I64.O.((I64.shift_left #1L (I64.to_int_trunc I64.O.(k - #1L))) - #1L) in
    luby_term I64.O.(n - prev_block))
;;

type t =
  { u : int64#
  ; mutable i : int64#
  }

let create ~unit_run = { u = unit_run; i = #1L }

let value t = I64.O.(t.u * luby_term t.i)

let next t =
  t.i <- I64.O.(t.i + #1L);
  value t
;;
