open! Core

let rec luby_term n =
  let rec find_k k =
    if Stdlib.Int64.compare (Stdlib.Int64.sub (Stdlib.Int64.shift_left 1L k) 1L) n < 0
    then find_k (k + 1)
    else k
  in
  let k = find_k 1 in
  if Stdlib.Int64.equal n (Stdlib.Int64.sub (Stdlib.Int64.shift_left 1L k) 1L)
  then Stdlib.Int64.shift_left 1L (k - 1)
  else (
    let prev_block = Stdlib.Int64.sub (Stdlib.Int64.shift_left 1L (k - 1)) 1L in
    luby_term (Stdlib.Int64.sub n prev_block))
;;

type t =
  { u : int64
  ; mutable i : int64
  }

let create ~unit_run = { u = unit_run; i = 1L }
let value t = Stdlib.Int64.mul t.u (luby_term t.i)

let next t =
  t.i <- Stdlib.Int64.add t.i 1L;
  value t
;;
