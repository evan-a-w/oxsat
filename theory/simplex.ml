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

  let map t ~f =
    match t with
    | Unbounded -> Unbounded
    | Bounded x -> Bounded (f x)
  ;;
end

module Bound = struct
  type t =
    { le : Q.t Maybe_bound.t
    ; ge : Q.t Maybe_bound.t
    }
  [@@deriving sexp]

  let check_bounds t with_q =
    let diff_with_bound bound =
      Maybe_bound.map bound ~f:(fun q -> Q.(q - with_q))
    in
    match diff_with_bound t.le with
    | Bounded q when Q.compare q Q.zero < 0 -> `Diff q
    | Unbounded | Bounded _ ->
      (match diff_with_bound t.ge with
       | Bounded q when Q.compare q Q.zero > 0 -> `Diff q
       | Unbounded | Bounded _ -> `In_bounds)
  ;;

  let in_bounds t with_q =
    match check_bounds t with_q with
    | `In_bounds -> true
    | `Diff _ -> false
  ;;

  let max t = t.le
  let min t = t.ge
end

module Var = struct
  type t =
    { mutable assignment : Q.t
    ; mutable bound : Bound.t
    ; id : int
    }
  [@@deriving sexp]

  let diff_to_become_in_bounds t =
    match Bound.check_bounds t.bound t.assignment with
    | `Diff q -> Some q
    | `In_bounds -> None
  ;;

  let allowed_to_shift t : Bound.t =
    let diff_with_bound bound =
      Maybe_bound.map bound ~f:(fun q -> Q.(q - t.assignment))
    in
    { le = diff_with_bound t.bound.le |> Maybe_bound.map ~f:(Q.max Q.zero)
    ; ge = diff_with_bound t.bound.ge |> Maybe_bound.map ~f:(Q.min Q.zero)
    }
  ;;

  let%expect_test "eg" =
    let t =
      { assignment = Q.zero
      ; bound = { le = Bounded (Q.of_int 10); ge = Bounded (Q.of_int 10) }
      ; id = 0
      }
    in
    print_s
      [%message
        (diff_to_become_in_bounds t : Q.t option) (allowed_to_shift t : Bound.t)];
    [%expect
      {|
      (("diff_to_become_in_bounds t" (((num 10) (den 1))))
       ("allowed_to_shift t"
        ((le (Bounded ((num 10) (den 1)))) (ge (Bounded ((num 0) (den 1)))))))
      |}];
    t.assignment <- Q.of_int 10;
    print_s
      [%message
        (diff_to_become_in_bounds t : Q.t option) (allowed_to_shift t : Bound.t)];
    [%expect
      {|
      (("diff_to_become_in_bounds t" ())
       ("allowed_to_shift t"
        ((le (Bounded ((num 0) (den 1)))) (ge (Bounded ((num 0) (den 1)))))))
      |}];
    t.assignment <- Q.of_int 20;
    print_s
      [%message
        (diff_to_become_in_bounds t : Q.t option) (allowed_to_shift t : Bound.t)];
    [%expect
      {|
      (("diff_to_become_in_bounds t" (((num -10) (den 1))))
       ("allowed_to_shift t"
        ((le (Bounded ((num 0) (den 1)))) (ge (Bounded ((num -10) (den 1)))))))
      |}]
  ;;
end

type t =
  { tableau : Q.t Vec.Value.t Vec.Value.t
  ; basic_vars : Var.t Vec.Value.t
  ; nonbasic_vars : Var.t Vec.Value.t
  ; vars : Var.t Vec.Value.t
  }
[@@deriving sexp]

(** returns the new var id *)
let add_nonbasic t : Var.t =
  let var : Var.t =
    { assignment = Q.zero
    ; bound = { le = Unbounded; ge = Unbounded }
    ; id = Vec.Value.length t.vars
    }
  in
  Vec.Value.push t.vars var;
  Vec.Value.push t.nonbasic_vars var;
  Vec.Value.iter t.tableau ~f:(fun v -> Vec.Value.push v Q.zero);
  var
;;

(** returns the new var id *)
let add_processed_constraint t ~nonbasic_coefficients ~bound =
  let var : Var.t =
    { assignment = Q.zero; bound; id = Vec.Value.length t.vars }
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
  let bound : Bound.t =
    let const = Q.( - ) rhs.const lhs.const in
    match op with
    | `Le -> { le = Bounded const; ge = Unbounded }
    | `Ge -> { le = Unbounded; ge = Bounded const }
    | `Eq -> { le = Bounded const; ge = Bounded const }
  in
  add_processed_constraint t ~nonbasic_coefficients ~bound
;;

let get_tableau t ~row ~col = Vec.Value.get (Vec.Value.get t.tableau row) col

let set_tableau t ~row ~col ~q =
  Vec.Value.set (Vec.Value.get t.tableau row) col q
;;

let eval t ~row =
  let sum = ref Q.zero in
  Vec.Value.iteri (Vec.Value.get t.tableau row) ~f:(fun i x ->
    let var = Vec.Value.get t.nonbasic_vars i in
    sum := Q.(!sum + (var.assignment * x)));
  !sum
;;

(** col = nonbasic var, row = basic var *)
let pivot t ~row ~col ~diff_to_col =
  let coeff = get_tableau t ~row ~col in
  assert (not (Q.is_zero coeff));
  set_tableau t ~row ~col ~q:(Q.neg Q.one);
  (* solve row for var [col] *)
  Vec.Value.map_inplace (Vec.Value.get t.tableau row) ~f:(fun q ->
    Q.( / ) (Q.neg q) coeff);
  (* add [row] to every other row to eliminate var [col] *)
  for i = 0 to Vec.Value.length t.tableau - 1 do
    let row' = Vec.Value.get t.tableau i in
    match i = row with
    | true -> ()
    | false ->
      let mult = Vec.Value.get row' col in
      let basic_var = Vec.Value.get t.basic_vars i in
      (* here we apply the diff to the var as well *)
      basic_var.assignment <- Q.((diff_to_col * mult) + basic_var.assignment);
      for j = 0 to Vec.Value.length row' - 1 do
        if j = col
        then Vec.Value.set row' j (Q.( * ) (get_tableau t ~row ~col) mult)
        else
          Vec.Value.set
            row'
            j
            Q.(Vec.Value.get row' j + (get_tableau t ~row ~col:j * mult))
      done
  done;
  let old_nonbasic = Vec.Value.get t.nonbasic_vars col in
  let old_basic = Vec.Value.get t.basic_vars row in
  Vec.Value.set t.basic_vars row old_nonbasic;
  Vec.Value.set t.nonbasic_vars col old_basic
;;

let%expect_test "pivot example" =
  let bound0 : Bound.t = { le = Unbounded; ge = Bounded (Q.of_int 0) } in
  let bound1 : Bound.t = { le = Unbounded; ge = Bounded (Q.of_int 1) } in
  let bound2 : Bound.t = { le = Unbounded; ge = Bounded (Q.of_int 2) } in
  let unbounded : Bound.t = { le = Unbounded; ge = Unbounded } in
  let b0 : Var.t = { assignment = Q.zero; bound = bound0; id = 0 } in
  let b1 : Var.t = { assignment = Q.zero; bound = bound1; id = 1 } in
  let b2 : Var.t = { assignment = Q.zero; bound = bound2; id = 2 } in
  let nb0 : Var.t = { assignment = Q.zero; bound = unbounded; id = 3 } in
  let nb1 : Var.t = { assignment = Q.zero; bound = unbounded; id = 4 } in
  let t =
    { tableau =
        Vec.Value.of_list
          [ Vec.Value.of_list [ Q.one; Q.one ]
          ; Vec.Value.of_list [ Q.of_int 2; Q.of_int (-1) ]
          ; Vec.Value.of_list [ Q.of_int (-1); Q.of_int 2 ]
          ]
    ; basic_vars = Vec.Value.of_list [ b0; b1; b2 ]
    ; nonbasic_vars = Vec.Value.of_list [ nb0; nb1 ]
    ; vars = Vec.Value.of_list [ b0; b1; b2; nb0; nb1 ]
    }
  in
  print_s [%sexp (t : t)];
  [%expect
    {|
    ((tableau
      ((((num 1) (den 1)) ((num 1) (den 1)))
       (((num 2) (den 1)) ((num -1) (den 1)))
       (((num -1) (den 1)) ((num 2) (den 1)))))
     (basic_vars
      (((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 0) (den 1)))))) (id 0))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 1) (den 1)))))) (id 1))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 2) (den 1)))))) (id 2))))
     (nonbasic_vars
      (((assignment ((num 0) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 3))
       ((assignment ((num 0) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 4))))
     (vars
      (((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 0) (den 1)))))) (id 0))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 1) (den 1)))))) (id 1))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 2) (den 1)))))) (id 2))
       ((assignment ((num 0) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 3))
       ((assignment ((num 0) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 4)))))
    |}];
  pivot t ~row:0 ~col:0 ~diff_to_col:Q.zero;
  print_s [%sexp (t : t)];
  [%expect
    {|
    ((tableau
      ((((num 1) (den 1)) ((num -1) (den 1)))
       (((num 2) (den 1)) ((num -3) (den 1)))
       (((num -1) (den 1)) ((num 3) (den 1)))))
     (basic_vars
      (((assignment ((num 0) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 3))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 1) (den 1)))))) (id 1))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 2) (den 1)))))) (id 2))))
     (nonbasic_vars
      (((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 0) (den 1)))))) (id 0))
       ((assignment ((num 0) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 4))))
     (vars
      (((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 0) (den 1)))))) (id 0))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 1) (den 1)))))) (id 1))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 2) (den 1)))))) (id 2))
       ((assignment ((num 0) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 3))
       ((assignment ((num 0) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 4)))))
    |}]
;;

let rec solve t =
  let failing_basic =
    Vec.Value.findi t.basic_vars ~f:(fun row var ->
      Var.diff_to_become_in_bounds var |> Option.map ~f:(Tuple3.create row var))
  in
  match failing_basic with
  | None -> `Sat
  | Some (row, basic_var, diff) ->
    let candidate_nonbasic =
      Vec.Value.findi t.nonbasic_vars ~f:(fun col nonbasic_var ->
        let q = get_tableau t ~row ~col in
        match Q.is_zero q with
        | true -> None
        | false ->
          let need_apply = Q.(diff / q) in
          (match
             Bound.in_bounds (Var.allowed_to_shift nonbasic_var) need_apply
           with
           | false -> None
           | true -> Some (col, nonbasic_var, need_apply)))
    in
    (match candidate_nonbasic with
     | None -> `Unsat
     | Some (col, nonbasic_var, need_apply) ->
       nonbasic_var.assignment <- Q.(nonbasic_var.assignment + need_apply);
       basic_var.assignment <- Q.(basic_var.assignment + diff);
       pivot t ~row ~col ~diff_to_col:need_apply;
       solve t)
;;

let%expect_test "example simplex" =
  let bound0 : Bound.t = { le = Unbounded; ge = Bounded (Q.of_int 2) } in
  let bound1 : Bound.t = { le = Unbounded; ge = Bounded (Q.of_int 0) } in
  let bound2 : Bound.t = { le = Unbounded; ge = Bounded (Q.of_int 1) } in
  let unbounded : Bound.t = { le = Unbounded; ge = Unbounded } in
  let b0 : Var.t = { assignment = Q.zero; bound = bound0; id = 2 } in
  let b1 : Var.t = { assignment = Q.zero; bound = bound1; id = 3 } in
  let b2 : Var.t = { assignment = Q.zero; bound = bound2; id = 4 } in
  let nb0 : Var.t = { assignment = Q.zero; bound = unbounded; id = 0 } in
  let nb1 : Var.t = { assignment = Q.zero; bound = unbounded; id = 1 } in
  let t =
    { tableau =
        Vec.Value.of_list
          [ Vec.Value.of_list [ Q.one; Q.one ]
          ; Vec.Value.of_list [ Q.of_int 2; Q.of_int (-1) ]
          ; Vec.Value.of_list [ Q.of_int (-1); Q.of_int 2 ]
          ]
    ; basic_vars = Vec.Value.of_list [ b0; b1; b2 ]
    ; nonbasic_vars = Vec.Value.of_list [ nb0; nb1 ]
    ; vars = Vec.Value.of_list [ nb0; nb1; b0; b1; b2 ]
    }
  in
  print_s [%sexp (t : t)];
  [%expect
    {|
    ((tableau
      ((((num 1) (den 1)) ((num 1) (den 1)))
       (((num 2) (den 1)) ((num -1) (den 1)))
       (((num -1) (den 1)) ((num 2) (den 1)))))
     (basic_vars
      (((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 2) (den 1)))))) (id 2))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 0) (den 1)))))) (id 3))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 1) (den 1)))))) (id 4))))
     (nonbasic_vars
      (((assignment ((num 0) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 0))
       ((assignment ((num 0) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 1))))
     (vars
      (((assignment ((num 0) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 0))
       ((assignment ((num 0) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 1))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 2) (den 1)))))) (id 2))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 0) (den 1)))))) (id 3))
       ((assignment ((num 0) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 1) (den 1)))))) (id 4)))))
    |}];
  (match solve t with
   | `Sat -> print_endline "sat"
   | `Unsat -> print_endline "unsat");
  print_s [%sexp (t : t)];
  [%expect
    {|
    sat
    ((tableau
      ((((num 2) (den 3)) ((num -1) (den 3)))
       (((num 1) (den 1)) ((num -1) (den 1)))
       (((num 1) (den 3)) ((num 1) (den 3)))))
     (basic_vars
      (((assignment ((num 1) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 0))
       ((assignment ((num 1) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 0) (den 1)))))) (id 3))
       ((assignment ((num 1) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 1))))
     (nonbasic_vars
      (((assignment ((num 2) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 2) (den 1)))))) (id 2))
       ((assignment ((num 1) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 1) (den 1)))))) (id 4))))
     (vars
      (((assignment ((num 1) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 0))
       ((assignment ((num 1) (den 1))) (bound ((le Unbounded) (ge Unbounded)))
        (id 1))
       ((assignment ((num 2) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 2) (den 1)))))) (id 2))
       ((assignment ((num 1) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 0) (den 1)))))) (id 3))
       ((assignment ((num 1) (den 1)))
        (bound ((le Unbounded) (ge (Bounded ((num 1) (den 1)))))) (id 4)))))
    |}]
;;
