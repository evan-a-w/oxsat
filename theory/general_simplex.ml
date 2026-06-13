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

module Atom_data = struct
  type t =
    { le : Q.t Maybe_bound.t
    ; ge : Q.t Maybe_bound.t
    }
  [@@deriving sexp]
end

type t =
  { tableau : Q.t Vec.Value.t Vec.Value.t
  ; mutable num_nonbasic : int
  ; basic_constraints : Atom_data.t Vec.Value.t
  }

(** returns the new var id *)
let add_nonbasic t : int =
  let res = t.num_nonbasic in
  t.num_nonbasic <- t.num_nonbasic + 1;
  Vec.Value.iter t.tableau ~f:(fun v -> Vec.Value.push v Q.zero);
  res
;;

(** returns the new var id *)
let add_processed_constraint
  t
  ~nonbasic_coefficients
  ~(basic_atom_data : Atom_data.t)
  =
  Vec.Value.push t.basic_constraints basic_atom_data;
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
  let nonbasic_coefficients = Array.init t.num_nonbasic ~f:(Fn.const Q.zero) in
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
  let basic_atom_data : Atom_data.t =
    let const = Q.( - ) rhs.const lhs.const in
    match op with
    | `Le -> { le = Bounded const; ge = Unbounded }
    | `Ge -> { le = Unbounded; ge = Bounded const }
    | `Eq -> { le = Bounded const; ge = Bounded const }
  in
  add_processed_constraint t ~nonbasic_coefficients ~basic_atom_data
;;
