open! Core

type t =
  { num : int
  ; den : int
  }
[@@deriving sexp, hash, equal]

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let reduce ~num ~den =
  if den = 0 then failwith "Q: zero denominator";
  let sign = if den < 0 then -1 else 1 in
  let num = Int.( * ) num sign in
  let den = Int.( * ) den sign in
  if num = 0
  then { num = 0; den = 1 }
  else (
    let g = gcd (Int.abs num) den in
    { num = Int.( / ) num g; den = Int.( / ) den g })
;;

let zero = { num = 0; den = 1 }
let one = { num = 1; den = 1 }
let of_int num = { num; den = 1 }

let ( + ) a b =
  reduce
    ~num:(Int.( + ) (Int.( * ) a.num b.den) (Int.( * ) b.num a.den))
    ~den:(Int.( * ) a.den b.den)
;;

let ( - ) a b =
  reduce
    ~num:(Int.( - ) (Int.( * ) a.num b.den) (Int.( * ) b.num a.den))
    ~den:(Int.( * ) a.den b.den)
;;

let ( * ) a b = reduce ~num:(Int.( * ) a.num b.num) ~den:(Int.( * ) a.den b.den)
let ( / ) a b = reduce ~num:(Int.( * ) a.num b.den) ~den:(Int.( * ) a.den b.num)
let neg a = { a with num = Int.neg a.num }
let abs a = { a with num = Int.abs a.num }
let sign a = Int.compare a.num 0
let is_zero a = a.num = 0

(* [a.den], [b.den] > 0, so cross-multiplying preserves order. *)
let compare a b = Int.compare (Int.( * ) a.num b.den) (Int.( * ) b.num a.den)
let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b
let to_float a = Float.( / ) (Float.of_int a.num) (Float.of_int a.den)
let num a = a.num
let den a = a.den
