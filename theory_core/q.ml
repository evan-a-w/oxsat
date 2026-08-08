open! Core
module B = Bigint

module Repr = struct
  type t =
    { num : B.t
    ; den : B.t
    }
  [@@deriving sexp]
end

module Num_den = struct
  type t =
    | Small of
        { num : int
        ; den : int
        }
    | Big of
        { num : B.t
        ; den : B.t
        }
  [@@deriving sexp]
end

type t =
  | Small of
      { num : int
      ; den : int
      }
  | Big of Repr.t

let int_min_value = Int.min_value
let int_max_value = Int.max_value
let int_abs_opt i = if i = int_min_value then None else Some (Int.abs i)
let int_neg_opt i = if i = int_min_value then None else Some (-i)
let rec int_gcd a b = if b = 0 then a else int_gcd b (a % b)

let int_add_opt a b =
  if (b > 0 && a > int_max_value - b) || (b < 0 && a < int_min_value - b)
  then None
  else Some (a + b)
;;

let int_sub_opt a b =
  match int_neg_opt b with
  | None -> None
  | Some b -> int_add_opt a b
;;

let int_mul_opt a b =
  match int_abs_opt a, int_abs_opt b with
  | Some abs_a, Some abs_b when abs_a = 0 || abs_b <= int_max_value / abs_a ->
    Some (a * b)
  | _ -> None
;;

let rec bigint_gcd a b = if B.(b = zero) then a else bigint_gcd b B.(a % b)

let small_int_of_bigint b =
  match B.to_int b with
  | Some i when i <> int_min_value -> Some i
  | Some _ | None -> None
;;

let small ~num ~den = Small { num; den }
let big ~num ~den = Big { num; den }

let reduce_big ~num ~den =
  if B.(den = zero) then failwith "Q: zero denominator";
  let num, den = if B.(den < zero) then B.neg num, B.neg den else num, den in
  if B.(num = zero)
  then small ~num:0 ~den:1
  else (
    let g = bigint_gcd (B.abs num) den in
    let num = B.(num / g) in
    let den = B.(den / g) in
    match small_int_of_bigint num, small_int_of_bigint den with
    | Some num, Some den -> small ~num ~den
    | _ -> big ~num ~den)
;;

let reduce_ints ~num ~den =
  if den = 0 then failwith "Q: zero denominator";
  let to_big () = reduce_big ~num:(B.of_int num) ~den:(B.of_int den) in
  let normalized =
    if den < 0
    then (
      match int_neg_opt num, int_neg_opt den with
      | Some num, Some den -> Some (num, den)
      | _ -> None)
    else Some (num, den)
  in
  match normalized with
  | None -> to_big ()
  | Some (num, den) ->
    if num = 0
    then small ~num:0 ~den:1
    else (
      match int_abs_opt num with
      | None -> to_big ()
      | Some abs_num ->
        let gcd = int_gcd abs_num den in
        small ~num:(num / gcd) ~den:(den / gcd))
;;

let zero = small ~num:0 ~den:1
let one = small ~num:1 ~den:1

let of_int num =
  if num = int_min_value
  then reduce_big ~num:(B.of_int num) ~den:B.one
  else small ~num ~den:1
;;

let of_bigint num = reduce_big ~num ~den:B.one
let of_int64 num = of_bigint (B.of_int64 num)

let num = function
  | Small { num; _ } -> B.of_int num
  | Big { num; _ } -> num
;;

let den = function
  | Small { den; _ } -> B.of_int den
  | Big { den; _ } -> den
;;

let num_den = function
  | Small { num; den } -> Num_den.Small { num; den }
  | Big { num; den } -> Big { num; den }
;;

let sexp_of_t t =
  let repr : Repr.t = { num = num t; den = den t } in
  Repr.sexp_of_t repr
;;

let t_of_sexp sexp =
  let { Repr.num; den } = Repr.t_of_sexp sexp in
  reduce_big ~num ~den
;;

let hash_fold_t state = function
  | Small { num; den } ->
    B.hash_fold_t (B.hash_fold_t state (B.of_int num)) (B.of_int den)
  | Big { num; den } -> B.hash_fold_t (B.hash_fold_t state num) den
;;

let hash t =
  Ppx_hash_lib.Std.Hash.get_hash_value
    (hash_fold_t (Ppx_hash_lib.Std.Hash.alloc ()) t)
;;

let is_integral = function
  | Small { den; _ } -> den = 1
  | Big { den; _ } -> B.(den = one)
;;

let is_zero = function
  | Small { num; _ } -> num = 0
  | Big { num; _ } -> B.(num = zero)
;;

let sign' = function
  | Small { num; _ } -> Int.compare num 0
  | Big { num; _ } -> B.compare num B.zero
;;

let sign a =
  let x = sign' a in
  if x < 0 then `Neg else if x = 0 then `Zero else `Pos
;;

let add_big a b =
  let a_num = num a in
  let a_den = den a in
  let b_num = num b in
  let b_den = den b in
  reduce_big ~num:B.((a_num * b_den) + (b_num * a_den)) ~den:B.(a_den * b_den)
;;

let ( + ) (local_ a) (local_ b) =
  match a, b with
  | Small { num = a_num; den = a_den }, Small { num = b_num; den = b_den } ->
    if a_den = b_den
    then (
      match int_add_opt a_num b_num with
      | Some num -> reduce_ints ~num ~den:a_den
      | None -> add_big a b)
    else (
      match
        ( int_mul_opt a_num b_den
        , int_mul_opt b_num a_den
        , int_mul_opt a_den b_den )
      with
      | Some left, Some right, Some den ->
        (match int_add_opt left right with
         | Some num -> reduce_ints ~num ~den
         | None -> add_big a b)
      | _ -> add_big a b)
  | _ -> add_big a b
;;

let sub_big a b =
  let a_num = num a in
  let a_den = den a in
  let b_num = num b in
  let b_den = den b in
  reduce_big ~num:B.((a_num * b_den) - (b_num * a_den)) ~den:B.(a_den * b_den)
;;

let ( - ) a b =
  match a, b with
  | Small { num = a_num; den = a_den }, Small { num = b_num; den = b_den } ->
    if a_den = b_den
    then (
      match int_sub_opt a_num b_num with
      | Some num -> reduce_ints ~num ~den:a_den
      | None -> sub_big a b)
    else (
      match
        ( int_mul_opt a_num b_den
        , int_mul_opt b_num a_den
        , int_mul_opt a_den b_den )
      with
      | Some left, Some right, Some den ->
        (match int_sub_opt left right with
         | Some num -> reduce_ints ~num ~den
         | None -> sub_big a b)
      | _ -> sub_big a b)
  | _ -> sub_big a b
;;

let mul_big a b = reduce_big ~num:B.(num a * num b) ~den:B.(den a * den b)

let ( * ) a b =
  match a, b with
  | Small { num = a_num; den = a_den }, Small { num = b_num; den = b_den } ->
    (match int_mul_opt a_num b_num, int_mul_opt a_den b_den with
     | Some num, Some den -> reduce_ints ~num ~den
     | _ -> mul_big a b)
  | _ -> mul_big a b
;;

let div_big a b = reduce_big ~num:B.(num a * den b) ~den:B.(den a * num b)

let ( / ) a b =
  match a, b with
  | Small { num = a_num; den = a_den }, Small { num = b_num; den = b_den } ->
    (match int_mul_opt a_num b_den, int_mul_opt a_den b_num with
     | Some num, Some den -> reduce_ints ~num ~den
     | _ -> div_big a b)
  | _ -> div_big a b
;;

let neg (local_ a) =
  match a with
  | Small { num; den } ->
    (match int_neg_opt num with
     | Some num -> small ~num ~den
     | None -> big ~num:(B.neg (B.of_int num)) ~den:(B.of_int den))
  | Big { num; den } -> reduce_big ~num:(B.neg num) ~den
;;

let abs a =
  match a with
  | Small { num; den } ->
    (match int_abs_opt num with
     | Some num -> small ~num ~den
     | None -> big ~num:(B.abs (B.of_int num)) ~den:(B.of_int den))
  | Big { num; den } -> big ~num:(B.abs num) ~den
;;

let compare_big a b = B.compare B.(num a * den b) B.(num b * den a)

let compare a b =
  match a, b with
  | Small { num = a_num; den = a_den }, Small { num = b_num; den = b_den } ->
    (match int_mul_opt a_num b_den, int_mul_opt b_num a_den with
     | Some left, Some right -> Int.compare left right
     | _ -> compare_big a b)
  | _ -> compare_big a b
;;

let equal a b = compare a b = 0
let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b

let to_float = function
  | Small { num; den } -> Float.( / ) (Float.of_int num) (Float.of_int den)
  | Big { num; den } -> Float.( / ) (B.to_float num) (B.to_float den)
;;

let floor_big { Repr.num; den } =
  let floor_num =
    if B.(num >= zero)
    then B.(num / den)
    else (
      let abs_num = B.abs num in
      let quotient = B.(abs_num / den) in
      let remainder = B.(abs_num % den) in
      B.neg (if B.(remainder = zero) then quotient else B.(quotient + one)))
  in
  reduce_big ~num:floor_num ~den:B.one
;;

let floor = function
  | Small { num; den } ->
    if num >= 0
    then small ~num:Int.(num / den) ~den:1
    else (
      match int_abs_opt num with
      | Some abs_num ->
        let quotient = Int.(abs_num / den) in
        let remainder = abs_num % den in
        small
          ~num:(-(if remainder = 0 then quotient else Int.(quotient + 1)))
          ~den:1
      | None -> floor_big { num = B.of_int num; den = B.of_int den })
  | Big repr -> floor_big repr
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

let%expect_test "small arithmetic falls back on overflow" =
  print_s [%sexp (of_int Int.max_value + one : t)];
  [%expect {| ((num 4611686018427387904) (den 1)) |}];
  print_s [%sexp (of_int Int.max_value * of_int 2 : t)];
  [%expect {| ((num 9223372036854775806) (den 1)) |}]
;;

let%expect_test "num_den exposes the small fast path" =
  print_s [%sexp (num_den (of_int 3 / of_int 2) : Num_den.t)];
  [%expect {| (Small (num 3) (den 2)) |}];
  print_s [%sexp (num_den (of_int64 Int64.max_value) : Num_den.t)];
  [%expect {| (Big (num 9223372036854775807) (den 1)) |}]
;;
