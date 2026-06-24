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

let is_integral { num = _; den } = den = 1
let zero = { num = 0; den = 1 }
let one = { num = 1; den = 1 }
let of_int num = { num; den = 1 }

let ( + ) (local_ a) (local_ b) =
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
let neg (local_ a) = { num = Int.neg a.num; den = a.den }
let abs a = { num = Int.abs a.num; den = a.den }
let sign' a = Int.compare a.num 0

let sign a =
  let x = sign' a in
  if x < 0 then `Neg else if x = 0 then `Zero else `Pos
;;

let is_zero a = a.num = 0

(* [a.den], [b.den] > 0, so cross-multiplying preserves order. *)
let compare a b = Int.compare (Int.( * ) a.num b.den) (Int.( * ) b.num a.den)
let ( > ) a b = compare a b > 0
let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b
let to_float a = Float.( / ) (Float.of_int a.num) (Float.of_int a.den)
let num a = a.num
let den a = a.den

let floor ({ num; den } as t) =
  if is_integral t
  then t
  else (
    let num' = Int.abs num in
    let r = num' mod den in
    if Int.O.(num' - r = 0)
    then zero
    else { num = Int.O.((num' - r) * (num / num') / den); den = 1 })
;;

let%expect_test "floor" =
  print_s [%sexp (of_int 1 / of_int 2 |> floor : t)];
  [%expect {| ((num 0) (den 1)) |}];
  print_s [%sexp (of_int (-1) / of_int 2 |> floor : t)];
  [%expect {| ((num 0) (den 1)) |}];
  print_s [%sexp (of_int 3 / of_int 2 |> floor : t)];
  [%expect {| ((num 1) (den 1)) |}];
  print_s [%sexp (of_int 3 / of_int (-2) |> floor : t)];
  [%expect {| ((num -1) (den 1)) |}];
  print_s [%sexp (of_int 84 / of_int 32 |> floor : t)];
  [%expect {| ((num 2) (den 1)) |}]
;;

let ceil t =
  if is_integral t then t else floor (t + of_int (Int.( * ) 1 (sign' t)))
;;

let%expect_test "ceil" =
  print_s [%sexp (of_int 1 / of_int 2 |> ceil : t)];
  [%expect {| ((num 1) (den 1)) |}];
  print_s [%sexp (of_int (-1) / of_int 2 |> ceil : t)];
  [%expect {| ((num -1) (den 1)) |}];
  print_s [%sexp (of_int 3 / of_int 2 |> ceil : t)];
  [%expect {| ((num 2) (den 1)) |}];
  print_s [%sexp (of_int 3 / of_int (-2) |> ceil : t)];
  [%expect {| ((num -2) (den 1)) |}];
  print_s [%sexp (of_int 84 / of_int 32 |> ceil : t)];
  [%expect {| ((num 3) (den 1)) |}]
;;
