open! Core
open! Feel.Import

module Op = struct
  type t =
    [ `Eq
    | `Le
    | `Ge
    | `Lt
    | `Gt
    ]
  [@@deriving sexp, compare, hash]
end

(* Represents [value + (eps_coeff * delta)], where [delta] is a fixed but
   symbolic infinitesimal. This is the standard trick (Dutertre & de Moura) for
   handling strict inequalities in a Simplex tableau that otherwise only
   supports non-strict bounds: [x < c] becomes the bound [x <= c - delta], and
   [x > c] becomes [x >= c + delta]. Ordering is lexicographic on
   [(value, eps_coeff)], which is correct for any sufficiently small
   [delta > 0]. *)
module Q_eps = struct
  type t =
    { value : Q.t
    ; eps_coeff : Q.t
    }
  [@@deriving sexp, compare, hash]

  let of_q value = { value; eps_coeff = Q.zero }
  let zero = of_q Q.zero

  let ( + ) a b =
    { value = Q.(a.value + b.value); eps_coeff = Q.(a.eps_coeff + b.eps_coeff) }
  ;;

  let ( - ) a b =
    { value = Q.(a.value - b.value); eps_coeff = Q.(a.eps_coeff - b.eps_coeff) }
  ;;

  let scale a ~by =
    { value = Q.(a.value * by); eps_coeff = Q.(a.eps_coeff * by) }
  ;;

  let max a b = if compare a b >= 0 then a else b
  let min a b = if compare a b <= 0 then a else b
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
    { le : Q_eps.t Maybe_bound.t
    ; ge : Q_eps.t Maybe_bound.t
    }
  [@@deriving sexp]

  let unbounded = { le = Unbounded; ge = Unbounded }

  let check_bounds t with_q =
    let diff_with_bound bound =
      Maybe_bound.map bound ~f:(fun q -> Q_eps.(q - with_q))
    in
    match diff_with_bound t.le with
    | Bounded q when Q_eps.compare q Q_eps.zero < 0 -> `Diff q
    | Unbounded | Bounded _ ->
      (match diff_with_bound t.ge with
       | Bounded q when Q_eps.compare q Q_eps.zero > 0 -> `Diff q
       | Unbounded | Bounded _ -> `In_bounds)
  ;;

  let in_bounds t with_q =
    match check_bounds t with_q with
    | `In_bounds -> true
    | `Diff _ -> false
  ;;
end

module Var = struct
  type t =
    { mutable assignment : Q_eps.t
    ; mutable bound : Bound.t
    ; id : int
    ; mutable where : [ `Basic of int | `Nonbasic of int ]
    }
  [@@deriving sexp]

  let diff_to_become_in_bounds t =
    match Bound.check_bounds t.bound t.assignment with
    | `Diff q -> Some q
    | `In_bounds -> None
  ;;

  let allowed_to_shift t : Bound.t =
    let diff_with_bound bound =
      Maybe_bound.map bound ~f:(fun q -> Q_eps.(q - t.assignment))
    in
    { le =
        diff_with_bound t.bound.le |> Maybe_bound.map ~f:(Q_eps.max Q_eps.zero)
    ; ge =
        diff_with_bound t.bound.ge |> Maybe_bound.map ~f:(Q_eps.min Q_eps.zero)
    }
  ;;

  let%expect_test "eg" =
    let t =
      { assignment = Q_eps.zero
      ; bound =
          { le = Bounded (Q_eps.of_q (Q.of_int 10))
          ; ge = Bounded (Q_eps.of_q (Q.of_int 10))
          }
      ; id = 0
      ; where = `Nonbasic 0
      }
    in
    print_s
      [%message
        (diff_to_become_in_bounds t : Q_eps.t option)
          (allowed_to_shift t : Bound.t)];
    [%expect
      {|
      (("diff_to_become_in_bounds t"
        (((value ((num 10) (den 1))) (eps_coeff ((num 0) (den 1))))))
       ("allowed_to_shift t"
        ((le (Bounded ((value ((num 10) (den 1))) (eps_coeff ((num 0) (den 1))))))
         (ge (Bounded ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))))))
      |}];
    t.assignment <- Q_eps.of_q (Q.of_int 10);
    print_s
      [%message
        (diff_to_become_in_bounds t : Q_eps.t option)
          (allowed_to_shift t : Bound.t)];
    [%expect
      {|
      (("diff_to_become_in_bounds t" ())
       ("allowed_to_shift t"
        ((le (Bounded ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))
         (ge (Bounded ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))))))
      |}];
    t.assignment <- Q_eps.of_q (Q.of_int 20);
    print_s
      [%message
        (diff_to_become_in_bounds t : Q_eps.t option)
          (allowed_to_shift t : Bound.t)];
    [%expect
      {|
      (("diff_to_become_in_bounds t"
        (((value ((num -10) (den 1))) (eps_coeff ((num 0) (den 1))))))
       ("allowed_to_shift t"
        ((le (Bounded ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))
         (ge (Bounded ((value ((num -10) (den 1))) (eps_coeff ((num 0) (den 1)))))))))
      |}]
  ;;
end

type t =
  { tableau : Q.t Vec.Value.t Vec.Value.t
  ; basic_vars : Var.t Vec.Value.t
  ; nonbasic_vars : Var.t Vec.Value.t
  ; vars : Var.t Vec.Value.t
  ; new_assignments : Q.t Int.Hash_queue.t
  }
[@@deriving sexp_of, fields]

let assign t ~(var : Var.t) ~q =
  var.assignment <- q;
  Hash_queue.replace_or_enqueue_front t.new_assignments var.id q.Q_eps.value
;;

type constraint_ = int [@@deriving sexp, compare, equal, hash]

let add_nonbasic t : Var.t =
  let var : Var.t =
    { assignment = Q_eps.zero
    ; bound = { le = Unbounded; ge = Unbounded }
    ; id = Vec.Value.length t.vars
    ; where = `Nonbasic (Vec.Value.length t.nonbasic_vars)
    }
  in
  Vec.Value.push t.vars var;
  Vec.Value.push t.nonbasic_vars var;
  Vec.Value.iter t.tableau ~f:(fun v -> Vec.Value.push v Q.zero);
  var
;;

let add_processed_constraint t ~nonbasic_coefficients ~bound =
  let var : Var.t =
    { assignment = Q_eps.zero
    ; bound
    ; id = Vec.Value.length t.vars
    ; where = `Basic (Vec.Value.length t.basic_vars)
    }
  in
  Vec.Value.push t.vars var;
  Vec.Value.push t.basic_vars var;
  Vec.Value.push
    t.tableau
    (Vec.Value.of_array_taking_ownership nonbasic_coefficients);
  var.id
;;

let add_constraint t ((lhs, op, rhs) : Q.t array * Op.t * Q.t) : constraint_ =
  let bound : Bound.t =
    match op with
    | `Le -> { le = Bounded (Q_eps.of_q rhs); ge = Unbounded }
    | `Ge -> { le = Unbounded; ge = Bounded (Q_eps.of_q rhs) }
    | `Eq -> { le = Bounded (Q_eps.of_q rhs); ge = Bounded (Q_eps.of_q rhs) }
    | `Lt ->
      { le = Bounded { value = rhs; eps_coeff = Q.neg Q.one }; ge = Unbounded }
    | `Gt -> { le = Unbounded; ge = Bounded { value = rhs; eps_coeff = Q.one } }
  in
  add_processed_constraint t ~nonbasic_coefficients:lhs ~bound
;;

let remove_constraint t ~constraint_ =
  (Vec.Value.get t.vars constraint_).bound <- Bound.unbounded
;;

let get_tableau t ~row ~col = Vec.Value.get (Vec.Value.get t.tableau row) col

let set_tableau t ~row ~col ~q =
  Vec.Value.set (Vec.Value.get t.tableau row) col q
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
      assign
        t
        ~var:basic_var
        ~q:Q_eps.(scale diff_to_col ~by:mult + basic_var.assignment);
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
  let basic_where = old_basic.where in
  old_basic.where <- old_nonbasic.where;
  old_nonbasic.where <- basic_where;
  Vec.Value.set t.nonbasic_vars col old_basic
;;

let%expect_test "pivot example" =
  let bound0 : Bound.t =
    { le = Unbounded; ge = Bounded (Q_eps.of_q (Q.of_int 0)) }
  in
  let bound1 : Bound.t =
    { le = Unbounded; ge = Bounded (Q_eps.of_q (Q.of_int 1)) }
  in
  let bound2 : Bound.t =
    { le = Unbounded; ge = Bounded (Q_eps.of_q (Q.of_int 2)) }
  in
  let unbounded : Bound.t = { le = Unbounded; ge = Unbounded } in
  let b0 : Var.t =
    { assignment = Q_eps.zero; bound = bound0; id = 0; where = `Basic 0 }
  in
  let b1 : Var.t =
    { assignment = Q_eps.zero; bound = bound1; id = 1; where = `Basic 1 }
  in
  let b2 : Var.t =
    { assignment = Q_eps.zero; bound = bound2; id = 2; where = `Basic 2 }
  in
  let nb0 : Var.t =
    { assignment = Q_eps.zero; bound = unbounded; id = 3; where = `Nonbasic 0 }
  in
  let nb1 : Var.t =
    { assignment = Q_eps.zero; bound = unbounded; id = 4; where = `Nonbasic 1 }
  in
  let t =
    { new_assignments = Int.Hash_queue.create ()
    ; tableau =
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
      (((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 0) (where (Basic 0)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 1) (where (Basic 1)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 2) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 2) (where (Basic 2)))))
     (nonbasic_vars
      (((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 3) (where (Nonbasic 0)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 4) (where (Nonbasic 1)))))
     (vars
      (((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 0) (where (Basic 0)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 1) (where (Basic 1)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 2) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 2) (where (Basic 2)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 3) (where (Nonbasic 0)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 4) (where (Nonbasic 1)))))
     (new_assignments ()))
    |}];
  pivot t ~row:0 ~col:0 ~diff_to_col:Q_eps.zero;
  print_s [%sexp (t : t)];
  [%expect
    {|
    ((tableau
      ((((num 1) (den 1)) ((num -1) (den 1)))
       (((num 2) (den 1)) ((num -3) (den 1)))
       (((num -1) (den 1)) ((num 3) (den 1)))))
     (basic_vars
      (((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 3) (where (Basic 0)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 1) (where (Basic 1)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 2) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 2) (where (Basic 2)))))
     (nonbasic_vars
      (((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 0) (where (Nonbasic 0)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 4) (where (Nonbasic 1)))))
     (vars
      (((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 0) (where (Nonbasic 0)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 1) (where (Basic 1)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 2) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 2) (where (Basic 2)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 3) (where (Basic 0)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 4) (where (Nonbasic 1)))))
     (new_assignments ((2 ((num 0) (den 1))) (1 ((num 0) (den 1))))))
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
          let need_apply = Q_eps.scale diff ~by:(Q.( / ) Q.one q) in
          (match
             Bound.in_bounds (Var.allowed_to_shift nonbasic_var) need_apply
           with
           | false -> None
           | true -> Some (col, nonbasic_var, need_apply)))
    in
    (match candidate_nonbasic with
     | None -> `Unsat
     | Some (col, nonbasic_var, need_apply) ->
       assign
         t
         ~var:nonbasic_var
         ~q:Q_eps.(nonbasic_var.assignment + need_apply);
       assign t ~var:basic_var ~q:Q_eps.(basic_var.assignment + diff);
       pivot t ~row ~col ~diff_to_col:need_apply;
       solve t)
;;

let%expect_test "example simplex" =
  let bound0 : Bound.t =
    { le = Unbounded; ge = Bounded (Q_eps.of_q (Q.of_int 2)) }
  in
  let bound1 : Bound.t =
    { le = Unbounded; ge = Bounded (Q_eps.of_q (Q.of_int 0)) }
  in
  let bound2 : Bound.t =
    { le = Unbounded; ge = Bounded (Q_eps.of_q (Q.of_int 1)) }
  in
  let unbounded : Bound.t = { le = Unbounded; ge = Unbounded } in
  let b0 : Var.t =
    { assignment = Q_eps.zero; bound = bound0; id = 2; where = `Basic 0 }
  in
  let b1 : Var.t =
    { assignment = Q_eps.zero; bound = bound1; id = 3; where = `Basic 1 }
  in
  let b2 : Var.t =
    { assignment = Q_eps.zero; bound = bound2; id = 4; where = `Basic 2 }
  in
  let nb0 : Var.t =
    { assignment = Q_eps.zero; bound = unbounded; id = 0; where = `Nonbasic 0 }
  in
  let nb1 : Var.t =
    { assignment = Q_eps.zero; bound = unbounded; id = 1; where = `Nonbasic 1 }
  in
  let t =
    { new_assignments = Int.Hash_queue.create ()
    ; tableau =
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
      (((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 2) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 2) (where (Basic 0)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 3) (where (Basic 1)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 4) (where (Basic 2)))))
     (nonbasic_vars
      (((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 0) (where (Nonbasic 0)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 1) (where (Nonbasic 1)))))
     (vars
      (((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 0) (where (Nonbasic 0)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 1) (where (Nonbasic 1)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 2) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 2) (where (Basic 0)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 3) (where (Basic 1)))
       ((assignment ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 4) (where (Basic 2)))))
     (new_assignments ()))
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
      (((assignment ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 0) (where (Basic 0)))
       ((assignment ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 3) (where (Basic 1)))
       ((assignment ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 1) (where (Basic 2)))))
     (nonbasic_vars
      (((assignment ((value ((num 2) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 2) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 2) (where (Nonbasic 0)))
       ((assignment ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 4) (where (Nonbasic 1)))))
     (vars
      (((assignment ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 0) (where (Basic 0)))
       ((assignment ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound ((le Unbounded) (ge Unbounded))) (id 1) (where (Basic 2)))
       ((assignment ((value ((num 2) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 2) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 2) (where (Nonbasic 0)))
       ((assignment ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 0) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 3) (where (Basic 1)))
       ((assignment ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1)))))
        (bound
         ((le Unbounded)
          (ge
           (Bounded ((value ((num 1) (den 1))) (eps_coeff ((num 0) (den 1))))))))
        (id 4) (where (Nonbasic 1)))))
     (new_assignments
      ((1 ((num 1) (den 1))) (4 ((num 1) (den 1))) (3 ((num 1) (den 1)))
       (2 ((num 2) (den 1))) (0 ((num 1) (den 1))))))
    |}]
;;

module Snapshot = struct
  type t = Q_eps.t array
end

let snapshot_assignments t : Snapshot.t =
  Array.init (Vec.Value.length t.vars) ~f:(fun i ->
    (Vec.Value.get t.vars i).assignment)
;;

let restore_assignments t (snapshot : Snapshot.t) =
  Array.iteri snapshot ~f:(fun i q -> (Vec.Value.get t.vars i).assignment <- q)
;;

let create () =
  { tableau = Vec.Value.create ()
  ; basic_vars = Vec.Value.create ()
  ; nonbasic_vars = Vec.Value.create ()
  ; vars = Vec.Value.create ()
  ; new_assignments = Int.Hash_queue.create ()
  }
;;

let add_var t = (add_nonbasic t).id
let assignment t ~var = (Vec.Value.get t.vars var).assignment.value

let fold_conflict_row t ~f =
  let failing =
    Vec.Value.findi t.basic_vars ~f:(fun (row : int) (var : Var.t) ->
      match Var.diff_to_become_in_bounds var with
      | None -> None
      | Some (diff : Q_eps.t) -> Some (row, var, diff))
  in
  match failing with
  | None -> None
  | Some (row, basic_var, diff) ->
    let needs_increase = Q_eps.compare diff Q_eps.zero > 0 in
    f ~var_id:basic_var.id ~bound_side:(if needs_increase then `Ge else `Le);
    Vec.Value.iteri t.nonbasic_vars ~f:(fun col nonbasic_var ->
      let coeff = get_tableau t ~row ~col in
      if not (Q.is_zero coeff)
      then (
        let need_le_witness =
          match needs_increase, Q.compare coeff Q.zero > 0 with
          | true, true | false, false -> true
          | true, false | false, true -> false
        in
        f
          ~var_id:nonbasic_var.id
          ~bound_side:(if need_le_witness then `Le else `Ge)));
    Some ()
;;

let add_constraint t (lhs, op, rhs) =
  let nonbasic_coefficients =
    Array.init (Vec.Value.length t.nonbasic_vars) ~f:(Fn.const Q.zero)
  in
  List.iter lhs ~f:(fun (q, i) ->
    let var = Vec.Value.get t.vars i in
    match var.where with
    | `Nonbasic x ->
      nonbasic_coefficients.(x) <- Q.(nonbasic_coefficients.(x) + q)
    | `Basic x ->
      Vec.Value.iteri (Vec.Value.get t.tableau x) ~f:(fun i q' ->
        nonbasic_coefficients.(i) <- Q.(nonbasic_coefficients.(i) + (q * q'))));
  add_constraint t (nonbasic_coefficients, op, rhs)
;;

let%expect_test "strict inequalities (`Lt / `Gt)" =
  (* x + y <= 10, x > 3, y > 2, x - y > 1. Satisfiable, e.g. x=3, y=2 sits
     exactly on the (open) corner of the x>3/y>2 region, which the eps-trick
     must still accept since real-valued strict inequalities have no minimal gap
     from their boundary. *)
  let t = create () in
  let x = add_var t in
  let y = add_var t in
  let sum = add_constraint t ([ Q.one, x; Q.one, y ], `Le, Q.of_int 10) in
  let x_gt_3 = add_constraint t ([ Q.one, x ], `Gt, Q.of_int 3) in
  let y_gt_2 = add_constraint t ([ Q.one, y ], `Gt, Q.of_int 2) in
  let diff_gt_1 = add_constraint t ([ Q.one, x; Q.neg Q.one, y ], `Gt, Q.one) in
  print_s [%sexp (solve t : [ `Sat | `Unsat ])];
  [%expect {| Sat |}];
  print_s [%message (assignment t ~var:x : Q.t) (assignment t ~var:y : Q.t)];
  [%expect
    {|
    (("assignment t ~var:x" ((num 3) (den 1)))
     ("assignment t ~var:y" ((num 2) (den 1))))
    |}];
  ignore (sum : constraint_);
  ignore (x_gt_3 : constraint_);
  ignore (y_gt_2 : constraint_);
  ignore (diff_gt_1 : constraint_)
;;

let%expect_test "`Gt conflicts with a non-strict `Le at the same bound" =
  (* x > 3 and x <= 3 together leave no room, unlike if `Gt had been
     (incorrectly) treated as `Ge, where x=3 would have stayed feasible. Both
     constraints are added before [solve] is ever called, so this isn't
     confounded by the (separate, pre-existing) issue where a basic var
     introduced by [add_constraint] after a [solve] starts from a stale zero
     assignment. *)
  let t = create () in
  let x = add_var t in
  let (_ : constraint_) = add_constraint t ([ Q.one, x ], `Gt, Q.of_int 3) in
  let (_ : constraint_) = add_constraint t ([ Q.one, x ], `Le, Q.of_int 3) in
  print_s [%sexp (solve t : [ `Sat | `Unsat ])];
  [%expect {| Unsat |}]
;;

let%expect_test "`Lt and `Gt together admit a strictly-between solution" =
  let t = create () in
  let x = add_var t in
  let (_ : constraint_) = add_constraint t ([ Q.one, x ], `Gt, Q.of_int 3) in
  let (_ : constraint_) = add_constraint t ([ Q.one, x ], `Lt, Q.of_int 4) in
  print_s [%sexp (solve t : [ `Sat | `Unsat ])];
  [%expect {| Sat |}]
;;
