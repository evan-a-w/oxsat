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

let rec bigint_gcd a b = if B.(b = zero) then a else bigint_gcd b B.(a % b)
let bigint_lcm a b = B.(a / bigint_gcd a b * b)

let primitive t =
  match is_zero t with
  | true -> t, Q.one
  | false ->
    let all = t.const :: Map.data t.coeffs in
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

include functor Comparable.Make
include functor Hashable.Make
