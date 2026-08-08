open! Core
module B = Bigint

type t =
  { num : B.t
  ; den : B.t
  }
[@@deriving sexp, hash, equal]

let rec gcd a b = if B.(b = zero) then a else gcd b B.(a % b)

let reduce ~num ~den =
  if B.(den = zero) then failwith "Q: zero denominator";
  let sign_factor = if B.(den < zero) then B.of_int (-1) else B.one in
  let num = B.( * ) num sign_factor in
  let den = B.( * ) den sign_factor in
  if B.(num = zero)
  then { num = B.zero; den = B.one }
  else (
    let g = gcd (B.abs num) den in
    { num = B.(num / g); den = B.(den / g) })
;;

let is_integral { num = _; den } = B.(den = one)
let zero = { num = B.zero; den = B.one }
let one = { num = B.one; den = B.one }
let of_bigint num = { num; den = B.one }
let of_int num = of_bigint (B.of_int num)
let of_int64 num = of_bigint (B.of_int64 num)

let ( + ) (local_ a) (local_ b) =
  reduce ~num:B.((a.num * b.den) + (b.num * a.den)) ~den:B.(a.den * b.den)
;;

let ( - ) a b =
  reduce ~num:B.((a.num * b.den) - (b.num * a.den)) ~den:B.(a.den * b.den)
;;

let ( * ) a b = reduce ~num:B.(a.num * b.num) ~den:B.(a.den * b.den)
let ( / ) a b = reduce ~num:B.(a.num * b.den) ~den:B.(a.den * b.num)
let neg (local_ a) = { num = B.neg a.num; den = a.den }
let abs a = { num = B.abs a.num; den = a.den }
let sign' a = B.compare a.num B.zero

let sign a =
  let x = sign' a in
  if x < 0 then `Neg else if x = 0 then `Zero else `Pos
;;

let is_zero a = B.(a.num = zero)

(* [a.den], [b.den] > 0, so cross-multiplying preserves order. *)
let compare a b = B.compare B.(a.num * b.den) B.(b.num * a.den)
let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b
let to_float a = Float.( / ) (B.to_float a.num) (B.to_float a.den)
let num a = a.num
let den a = a.den

let floor { num; den } =
  let floor_num =
    if B.(num >= zero)
    then B.(num / den)
    else (
      let abs_num = B.abs num in
      let quotient = B.(abs_num / den) in
      let remainder = B.(abs_num % den) in
      B.neg (if B.(remainder = zero) then quotient else B.(quotient + one)))
  in
  { num = floor_num; den = B.one }
;;

let%expect_test "floor" =
  print_s [%sexp (of_int 1 / of_int 2 |> floor : t)];
  [%expect {| ((num 0) (den 1)) |}];
  print_s [%sexp (of_int (-1) / of_int 2 |> floor : t)];
  [%expect {| ((num -1) (den 1)) |}];
  print_s [%sexp (of_int 3 / of_int 2 |> floor : t)];
  [%expect {| ((num 1) (den 1)) |}];
  print_s [%sexp (of_int 3 / of_int (-2) |> floor : t)];
  [%expect {| ((num -2) (den 1)) |}];
  print_s [%sexp (of_int 84 / of_int 32 |> floor : t)];
  [%expect {| ((num 2) (den 1)) |}]
;;

let ceil t = neg (floor (neg t))

let%expect_test "ceil" =
  print_s [%sexp (of_int 1 / of_int 2 |> ceil : t)];
  [%expect {| ((num 1) (den 1)) |}];
  print_s [%sexp (of_int (-1) / of_int 2 |> ceil : t)];
  [%expect {| ((num 0) (den 1)) |}];
  print_s [%sexp (of_int 3 / of_int 2 |> ceil : t)];
  [%expect {| ((num 2) (den 1)) |}];
  print_s [%sexp (of_int 3 / of_int (-2) |> ceil : t)];
  [%expect {| ((num -1) (den 1)) |}];
  print_s [%sexp (of_int 84 / of_int 32 |> ceil : t)];
  [%expect {| ((num 3) (den 1)) |}]
;;
