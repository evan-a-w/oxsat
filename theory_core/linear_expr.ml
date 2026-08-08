open! Core
open! Feel.Import

type t =
  { coeffs : Q.t Map.M(Tvar).t
  ; const : Q.t
  }
[@@deriving sexp, compare, hash]

let zero = { coeffs = Tvar.Map.empty; const = Q.zero }
let const const = { coeffs = Tvar.Map.empty; const }
let var v = { coeffs = Tvar.Map.singleton v Q.one; const = Q.zero }

let simplify t =
  { t with coeffs = Map.filter t.coeffs ~f:(fun q -> not (Q.is_zero q)) }
;;

let ( + ) a b =
  let coeffs =
    Map.merge a.coeffs b.coeffs ~f:(fun ~key:_ -> function
      | `Left q | `Right q -> Some q
      | `Both (q1, q2) -> Some Q.(q1 + q2))
  in
  simplify { coeffs; const = Q.(a.const + b.const) }
;;

let scale q t =
  match Q.is_zero q with
  | true -> zero
  | false ->
    { coeffs = Map.map t.coeffs ~f:(fun c -> Q.(q * c))
    ; const = Q.(q * t.const)
    }
;;

let neg t = scale (Q.of_int (-1)) t
let ( - ) a b = a + neg b
let is_zero t = Map.is_empty t.coeffs && Q.is_zero t.const

module B = Bigint

let rec int_gcd a b = if b = 0 then a else int_gcd b (a % b)

let int_mul_opt a b =
  let abs_opt i = if i = Int.min_value then None else Some (Int.abs i) in
  match abs_opt a, abs_opt b with
  | Some abs_a, Some abs_b when abs_a = 0 || abs_b <= Int.max_value / abs_a ->
    Some (a * b)
  | _ -> None
;;

let int_lcm_opt a b =
  let gcd = int_gcd a b in
  int_mul_opt (a / gcd) b
;;

let rec bigint_gcd a b = if B.(b = zero) then a else bigint_gcd b B.(a % b)
let bigint_lcm a b = B.(a / bigint_gcd a b * b)

let primitive_big t all =
  let d =
    List.fold all ~init:B.one ~f:(fun acc q -> bigint_lcm acc (Q.den q))
  in
  let scaled = scale (Q.of_bigint d) t in
  let g =
    List.fold
      (scaled.const :: Map.data scaled.coeffs)
      ~init:B.zero
      ~f:(fun acc q -> bigint_gcd acc (B.abs (Q.num q)))
  in
  let t' = scale Q.(one / of_bigint g) scaled in
  let factor = Q.(of_bigint g / of_bigint d) in
  t', factor
;;

let primitive_small t all =
  let open Option.Let_syntax in
  let%bind d =
    List.fold_until
      all
      ~init:(Some 1)
      ~f:(fun acc q ->
        match acc, Q.num_den q with
        | Some acc, Q.Num_den.Small { den; _ } -> Continue (int_lcm_opt acc den)
        | (None | Some _), Big _ -> Stop None
        | None, Small _ -> Continue None)
      ~finish:Fn.id
  in
  let scaled = scale (Q.of_int d) t in
  let%map g =
    List.fold_until
      (scaled.const :: Map.data scaled.coeffs)
      ~init:(Some 0)
      ~f:(fun acc q ->
        match acc, Q.num_den q with
        | Some acc, Q.Num_den.Small { num; _ } ->
          (match Int.abs num with
           | exception _ -> Stop None
           | abs_num -> Continue (Some (int_gcd acc abs_num)))
        | (None | Some _), Big _ -> Stop None
        | None, Small _ -> Continue None)
      ~finish:Fn.id
  in
  let t' = scale Q.(one / of_int g) scaled in
  let factor = Q.(of_int g / of_int d) in
  t', factor
;;

let primitive t =
  match is_zero t with
  | true -> t, Q.one
  | false ->
    let all = t.const :: Map.data t.coeffs in
    (match primitive_small t all with
     | Some result -> result
     | None -> primitive_big t all)
;;

include functor Comparable.Make
include functor Hashable.Make
