open! Core
open! Feel.Import

module Sum = struct
  type t =
    { vars : (Q.t * int) Iarray.t
    ; const : Q.t
    }
  [@@deriving sexp, compare, hash]
end

module Op = struct
  type t =
    [ `Eq
    | `Le
    | `Ge
    ]
  [@@deriving sexp, compare, hash]
end

module Maybe_bound = struct
  type 'a t =
    | Unbounded
    | Bounded of 'a
  [@@deriving sexp, compare, hash]
end

module Var = struct
  type t =
    { mutable assignment : Q.t
    ; mutable kind : [ `Basic of int | `Nonbasic of int ]
    ; mutable le : Q.t Maybe_bound.t
    ; mutable ge : Q.t Maybe_bound.t
    }
  [@@deriving sexp]
end

type t =
  { tableau : Q.t Vec.Value.t Vec.Value.t
  ; basic_vars : Var.t Vec.Value.t
  ; nonbasic_vars : Var.t Vec.Value.t
  ; vars : Var.t Vec.Value.t
  }

(** returns the new var id *)
let add_nonbasic t : Var.t =
  let var : Var.t =
    { assignment = Q.zero
    ; kind = `Nonbasic (Vec.Value.length t.nonbasic_vars)
    ; le = Unbounded
    ; ge = Unbounded
    }
  in
  Vec.Value.push t.nonbasic_vars var;
  Vec.Value.push t.vars var;
  Vec.Value.iter t.tableau ~f:(fun v -> Vec.Value.push v Q.zero);
  var
;;

(** returns the new var id *)
let add_processed_constraint t ~nonbasic_coefficients ~le ~ge =
  let var : Var.t =
    { assignment = Q.zero
    ; kind = `Basic (Vec.Value.length t.basic_vars)
    ; le
    ; ge
    }
  in
  Vec.Value.push t.vars var;
  Vec.Value.push t.basic_vars var;
  Vec.Value.push
    t.tableau
    (Vec.Value.of_array_taking_ownership nonbasic_coefficients)
;;

let add_constraint t ((lhs, op, rhs) : Sum.t * Op.t * Sum.t) =
  let lhs_sorted_vars =
    Iarray.sort lhs.vars ~compare:(fun (_, var1) (_, var2) ->
      Int.compare var1 var2)
  in
  let rhs_sorted_vars =
    Iarray.sort lhs.vars ~compare:(fun (_, var1) (_, var2) ->
      Int.compare var1 var2)
  in
  let nonbasic_coefficients =
    Array.init (Vec.Value.length t.nonbasic_vars) ~f:(Fn.const Q.zero)
  in
  let set (q, v) = nonbasic_coefficients.(v) <- q in
  let rec collect_vars li ri =
    let ld = li >= Iarray.length lhs_sorted_vars in
    let rd = ri >= Iarray.length rhs_sorted_vars in
    if ld && rd
    then ()
    else if ld
    then (
      set (Iarray.get rhs_sorted_vars ri);
      collect_vars li (ri + 1))
    else if rd
    then (
      set (Iarray.get lhs_sorted_vars li);
      collect_vars (li + 1) ri)
    else (
      let lq, lv = Iarray.get lhs_sorted_vars li in
      let rq, rv = Iarray.get rhs_sorted_vars ri in
      match Ordering.of_int (Int.compare lv rv) with
      | Equal ->
        set (Q.( - ) lq rq, lv);
        collect_vars (li + 1) (ri + 1)
      | Greater ->
        set (rq, rv);
        collect_vars li (ri + 1)
      | Less ->
        set (lq, lv);
        collect_vars (li + 1) ri)
  in
  collect_vars 0 0;
  let (le, ge) : Q.t Maybe_bound.t * Q.t Maybe_bound.t =
    let const = Q.( - ) rhs.const lhs.const in
    match op with
    | `Le -> Bounded const, Unbounded
    | `Ge -> Unbounded, Bounded const
    | `Eq -> Bounded const, Bounded const
  in
  add_processed_constraint t ~nonbasic_coefficients ~le ~ge
;;
