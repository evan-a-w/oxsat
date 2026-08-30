open! Core
open! Feel.Import
open! Theory_core
open! Theory

let v name : Formula.any = Var (Tvar.of_string name)
let a = v "a"
let b = v "b"
let i = v "i"
let j = v "j"
let value = v "value"
let other = v "other"
let select array index : Formula.any = Select (array, index)
let store array index value : Formula.any = Store (array, index, value)
let eq left right : Formula.any = Eq (left, right)
let neq left right : Formula.any = Not (eq left right)

let forall (bound, triggers, body) : Formula.quantified =
  Forall
    ( bound
    , List.map triggers ~f:(List.map ~f:Formula.widen_quantified)
    , Formula.widen_quantified body )
;;

let has_int_int_array_type var : Formula.any =
  eq (Type_var var) (Array_type (Int, Int))
;;

let assert_ok solver formula =
  match Or_error.ok_exn (Solver.assert_formula solver formula) with
  | `Ok -> ()
  | `Unsat _ -> print_endline "Unsat at assert time"
;;

let print_solver_result = function
  | Solver_result.Unsat _ -> print_endline "Unsat"
  | Sat _ -> print_endline "Sat"
;;

let print_proof_result = function
  | Solver_result.Sat _ -> print_endline "Sat"
  | Unsat { proof = Some proof; _ } ->
    print_s
      [%message "Unsat" ~proof_check:(Proof.check proof : unit Or_error.t)]
  | Unsat { proof = None; _ } -> print_endline "Unsat without proof"
;;

let print_quantifier_proof_result = function
  | Quantifier_solver.Result.Sat _ -> print_endline "Sat"
  | Quantifier_solver.Result.Unknown_but_possibly_sat _ ->
    print_endline "Unknown"
  | Quantifier_solver.Result.Unsat { proof = Some proof; _ } ->
    print_s
      [%message "Unsat" ~proof_check:(Proof.check proof : unit Or_error.t)]
  | Quantifier_solver.Result.Unsat { proof = None; _ } ->
    print_endline "Unsat without proof"
;;

let mask_fresh_witnesses text =
  (* [Fresh_tvar.create ~hint] names [hint.<n>] where [<n>] is a process-global
     counter (and [Tvar.to_string] strips the generated suffix), so the counter
     is unstable. Mask it, keeping the hint, for stable expect output. *)
  let mask_hint text ~hint =
    let pattern = hint ^ "." in
    let rec skip_digits i =
      if i < String.length text && Char.is_digit text.[i]
      then skip_digits (i + 1)
      else i
    in
    let rec loop acc pos =
      match String.substr_index text ~pattern ~pos with
      | None -> acc ^ String.sub text ~pos ~len:(String.length text - pos)
      | Some index ->
        let digits_start = index + String.length pattern in
        let digits_end = skip_digits digits_start in
        if digits_end = digits_start
        then
          (* Not followed by a counter: copy the pattern verbatim. *)
          loop
            (acc ^ String.sub text ~pos ~len:(digits_start - pos))
            digits_start
        else (
          let acc =
            acc ^ String.sub text ~pos ~len:(index - pos) ^ pattern ^ "<fresh>"
          in
          loop acc digits_end)
    in
    loop "" 0
  in
  text |> mask_hint ~hint:"array_extensionality" |> mask_hint ~hint:".bound"
;;

(* Prints the [Proof.check] result and only the [Proof.to_string_hum] lines that
   explain the array theory reasoning, so expect output stays short. *)
let print_array_proof_lines proof =
  let lines =
    Proof.to_string_hum proof |> mask_fresh_witnesses |> String.split_lines
  in
  List.iter lines ~f:(fun line ->
    if String.is_substring line ~substring:"array" then print_endline line)
;;

let theory_literal atom ~positive =
  Proof.Literal.create ~atom:(Proof.Atom.Theory atom) ~positive
;;

let clause_exn literals =
  match Proof.Clause.create literals with
  | `Clause clause -> clause
  | `Tautology -> failwith "unexpected tautology"
;;

let%expect_test "read over write at the same index" =
  let solver = Solver.create () in
  assert_ok solver (neq (select (store a i value) i) value);
  print_solver_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "array lemmas have checkable proof certificates" =
  let solver =
    Solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  assert_ok solver (neq (select (store a i value) i) value);
  print_proof_result (Solver.solve solver);
  [%expect {| (Unsat (proof_check (Ok ()))) |}]
;;

let%expect_test "read over write at a different index" =
  let solver = Solver.create () in
  assert_ok solver (neq i j);
  assert_ok solver (neq (select (store a i value) j) (select a j));
  print_solver_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "scoped row lemma premise can be popped" =
  let solver = Solver.create () in
  assert_ok solver (neq (select (store a i value) j) (select a j));
  Solver.push solver;
  assert_ok solver (neq i j);
  print_solver_result (Solver.solve solver);
  Solver.pop solver;
  print_solver_result (Solver.solve solver);
  [%expect {|
    Unsat
    Sat
    |}]
;;

let%expect_test "scoped array type premise does not leak through retained \
                 lemmas"
  =
  let solver = Solver.create () in
  let b_var = Tvar.of_string "b" in
  assert_ok solver (neq a b);
  assert_ok solver (eq (select a i) (select a i));
  Solver.push solver;
  assert_ok solver (has_int_int_array_type b_var);
  print_solver_result (Solver.solve solver);
  print_s
    [%message
      "in scope" ~type_:(Solver.get_type solver b_var : Type_expr.t option)];
  Solver.pop solver;
  assert_ok solver (Not (has_int_int_array_type b_var));
  print_solver_result (Solver.solve solver);
  [%expect
    {|
    Sat
    ("in scope" (type_ ((Array_type (Base Int) (Base Int)))))
    Sat
    |}]
;;

let%expect_test "array extensionality with a universal select equality" =
  let solver = Quantifier_solver.create () in
  let k = Tvar.of_string "k" in
  ignore
    (Quantifier_solver.assert_formula
       solver
       (forall
          ( [ k ]
          , [ [ select a (Var k) ]; [ select b (Var k) ] ]
          , eq (select a (Var k)) (select b (Var k)) ))
     : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       solver
       (Formula.widen_quantified (eq (select a i) (select a i)))
     : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       solver
       (Formula.widen_quantified (eq (select b i) (select b i)))
     : _ Or_error.t);
  ignore
    (Quantifier_solver.assert_formula
       solver
       (Formula.widen_quantified (neq a b))
     : _ Or_error.t);
  (match Quantifier_solver.solve solver ~max_rounds:4 with
   | Unsat _ -> print_endline "Unsat"
   | Sat _ -> print_endline "Sat"
   | Unknown_but_possibly_sat _ -> print_endline "Unknown");
  [%expect {| Unsat |}]
;;

let%expect_test "satisfiable array problem has a checkable model" =
  let solver = Solver.create () in
  assert_ok solver (neq i j);
  assert_ok solver (eq (select (store a i value) j) other);
  (match Solver.solve solver with
   | Unsat _ -> print_endline "Unsat"
   | Sat { model } ->
     print_endline "Sat";
     print_s [%sexp (Solver.check_model solver model : unit Or_error.t)]);
  [%expect {|
    Sat
    (Ok ())
    |}]
;;

let%expect_test "array indices interact with linear arithmetic" =
  let solver = Solver.create () in
  assert_ok solver (La_compare (i, `Lt, j));
  assert_ok solver (neq (select (store a i value) j) (select a j));
  print_solver_result (Solver.solve solver);
  [%expect {| Unsat |}]
;;

let%expect_test "array indices interact with linear arithmetic with proofs" =
  let solver =
    Solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  assert_ok solver (La_compare (i, `Lt, j));
  assert_ok solver (neq (select (store a i value) j) (select a j));
  print_proof_result (Solver.solve solver);
  [%expect {| (Unsat (proof_check (Ok ()))) |}]
;;

let%expect_test "distinct arrays with a differing read stay satisfiable" =
  let solver = Solver.create () in
  assert_ok solver (neq a b);
  assert_ok solver (neq (select a i) (select b i));
  (match Solver.solve solver with
   | Unsat _ -> print_endline "Unsat"
   | Sat { model } ->
     print_endline "Sat";
     print_s [%sexp (Solver.check_model solver model : unit Or_error.t)]);
  [%expect {|
    Sat
    (Ok ())
    |}]
;;

let%expect_test "extensionality through aliased array variables" =
  let solver = Quantifier_solver.create () in
  let x = v "x" in
  let y = v "y" in
  let k = Tvar.of_string "k" in
  let assert_q f =
    ignore (Quantifier_solver.assert_formula solver f : _ Or_error.t)
  in
  assert_q
    (forall
       ( [ k ]
       , [ [ select x (Var k) ]; [ select y (Var k) ] ]
       , eq (select x (Var k)) (select y (Var k)) ));
  assert_q (Formula.widen_quantified (eq (select x i) (select x i)));
  assert_q (Formula.widen_quantified (eq (select y i) (select y i)));
  assert_q (Formula.widen_quantified (eq x a));
  assert_q (Formula.widen_quantified (eq y b));
  assert_q (Formula.widen_quantified (neq a b));
  (match Quantifier_solver.solve solver ~max_rounds:6 with
   | Unsat _ -> print_endline "Unsat"
   | Sat _ -> print_endline "Sat"
   | Unknown_but_possibly_sat _ -> print_endline "Unknown");
  [%expect {| Unsat |}]
;;

let%expect_test "extensionality through aliased array variables with proofs" =
  let solver =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let x = v "x" in
  let y = v "y" in
  let k = Tvar.of_string "k" in
  let assert_q f =
    ignore (Quantifier_solver.assert_formula solver f : _ Or_error.t)
  in
  assert_q
    (forall
       ( [ k ]
       , [ [ select x (Var k) ]; [ select y (Var k) ] ]
       , eq (select x (Var k)) (select y (Var k)) ));
  assert_q (Formula.widen_quantified (eq (select x i) (select x i)));
  assert_q (Formula.widen_quantified (eq (select y i) (select y i)));
  assert_q (Formula.widen_quantified (eq x a));
  assert_q (Formula.widen_quantified (eq y b));
  assert_q (Formula.widen_quantified (neq a b));
  print_quantifier_proof_result (Quantifier_solver.solve solver ~max_rounds:6);
  [%expect {| (Unsat (proof_check (Ok ()))) |}]
;;

let%expect_test "extensionality uses declared array types" =
  let solver = Quantifier_solver.create () in
  let b_var = Tvar.of_string "b" in
  let k = Tvar.of_string "k" in
  let assert_q f =
    ignore (Quantifier_solver.assert_formula solver f : _ Or_error.t)
  in
  assert_q
    (forall
       ( [ k ]
       , [ [ select b (Var k) ] ]
       , eq (select a (Var k)) (select b (Var k)) ));
  assert_q (Formula.widen_quantified (has_int_int_array_type b_var));
  assert_q (Formula.widen_quantified (eq (select a i) (select a i)));
  assert_q (Formula.widen_quantified (neq a b));
  (match Quantifier_solver.solve solver ~max_rounds:6 with
   | Unsat _ -> print_endline "Unsat"
   | Sat _ -> print_endline "Sat"
   | Unknown_but_possibly_sat _ -> print_endline "Unknown");
  [%expect {| Unsat |}]
;;

let%expect_test "extensionality uses declared array types with proofs" =
  let solver =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let b_var = Tvar.of_string "b" in
  let k = Tvar.of_string "k" in
  let assert_q f =
    ignore (Quantifier_solver.assert_formula solver f : _ Or_error.t)
  in
  assert_q
    (forall
       ( [ k ]
       , [ [ select b (Var k) ] ]
       , eq (select a (Var k)) (select b (Var k)) ));
  assert_q (Formula.widen_quantified (has_int_int_array_type b_var));
  assert_q (Formula.widen_quantified (eq (select a i) (select a i)));
  assert_q (Formula.widen_quantified (neq a b));
  print_quantifier_proof_result (Quantifier_solver.solve solver ~max_rounds:6);
  [%expect {| Unsat without proof |}]
;;

let%expect_test "client variable named like the old array witness does not \
                 collide"
  =
  let solver = Solver.create () in
  let b_var = Tvar.of_string "b" in
  let client_index = v "__array_extensionality_0" in
  assert_ok solver (eq (select a i) (select a i));
  assert_ok solver (neq a b);
  assert_ok solver (has_int_int_array_type b_var);
  assert_ok solver (eq (select a client_index) (select b client_index));
  print_solver_result (Solver.solve solver);
  [%expect {| Sat |}]
;;

let%expect_test "row1 proof prints human-readable certificate text" =
  let solver =
    Solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  assert_ok solver (neq (select (store a i value) i) value);
  (match Solver.solve solver with
   | Sat _ -> print_endline "sat"
   | Unsat { proof = Some proof; _ } ->
     print_endline (Proof.to_string_hum proof)
   | Unsat { proof = None; _ } -> print_endline "no proof produced");
  [%expect
    {|
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ real
      a2: bool ≠ int64
      a3: int ≠ real
      a4: int ≠ int64
      a5: real ≠ int64
      a6: select(store(a, i, value), i) ≠ value
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ real   [assumption a1]
      s2: bool ≠ int64   [assumption a2]
      s3: int ≠ real   [assumption a3]
      s4: int ≠ int64   [assumption a4]
      s5: real ≠ int64   [assumption a5]
      s6: select(store(a, i, value), i) ≠ value   [assumption a6]
      s7: false   [refutation of [s0, s1, s2, s3, s4, s5, s6]]
        refutation:
          steps:
            r0: value ≠ select(store(a, i, value), i)   [s6]
            r1: value = select(store(a, i, value), i)   [array row1: select(store(a, i, value), i) = value]
            r2: ⊥   [RUP over [r0, r1]]
    Conclusion: s7
    |}]
;;

let%expect_test "row2 proof prints human-readable certificate text" =
  let solver =
    Solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  assert_ok solver (neq i j);
  assert_ok solver (neq (select (store a i value) j) (select a j));
  (match Solver.solve solver with
   | Sat _ -> print_endline "sat"
   | Unsat { proof = Some proof; _ } ->
     print_endline (Proof.to_string_hum proof)
   | Unsat { proof = None; _ } -> print_endline "no proof produced");
  [%expect
    {|
    Assumptions:
      a0: bool ≠ int
      a1: bool ≠ real
      a2: bool ≠ int64
      a3: int ≠ real
      a4: int ≠ int64
      a5: real ≠ int64
      a6: i ≠ j
      a7: select(store(a, i, value), j) ≠ select(a, j)
    Steps:
      s0: bool ≠ int   [assumption a0]
      s1: bool ≠ real   [assumption a1]
      s2: bool ≠ int64   [assumption a2]
      s3: int ≠ real   [assumption a3]
      s4: int ≠ int64   [assumption a4]
      s5: real ≠ int64   [assumption a5]
      s6: i ≠ j   [assumption a6]
      s7: select(store(a, i, value), j) ≠ select(a, j)   [assumption a7]
      s8: false   [refutation of [s0, s1, s2, s3, s4, s5, s6, s7]]
        refutation:
          steps:
            r0: i ≠ j   [s6]
            r1: select(a, j) ≠ select(store(a, i, value), j)   [s7]
            r2: i = j ∨ select(a, j) = select(store(a, i, value), j)   [array row2: i ≠ j ⟹ select(store(a, i, value), j) = select(a, j)]
            r3: ⊥   [RUP over [r0, r1, r2]]
    Conclusion: s8
    |}]
;;

let%expect_test "extensionality proof prints certificate lines" =
  let solver =
    Quantifier_solver.create
      ~config:{ Solver.Config.default with produce_proofs = true }
      ()
  in
  let x = v "x" in
  let y = v "y" in
  let k = Tvar.of_string "k" in
  let assert_q f =
    ignore (Quantifier_solver.assert_formula solver f : _ Or_error.t)
  in
  assert_q
    (forall
       ( [ k ]
       , [ [ select x (Var k) ]; [ select y (Var k) ] ]
       , eq (select x (Var k)) (select y (Var k)) ));
  assert_q (Formula.widen_quantified (eq (select x i) (select x i)));
  assert_q (Formula.widen_quantified (eq (select y i) (select y i)));
  assert_q (Formula.widen_quantified (eq x a));
  assert_q (Formula.widen_quantified (eq y b));
  assert_q (Formula.widen_quantified (neq a b));
  (match Quantifier_solver.solve solver ~max_rounds:6 with
   | Unsat { proof = Some proof; _ } ->
     print_s [%message "check" ~result:(Proof.check proof : unit Or_error.t)];
     print_array_proof_lines proof
   | Sat _ -> print_endline "sat"
   | Unknown_but_possibly_sat _ -> print_endline "unknown"
   | Unsat { proof = None; _ } -> print_endline "no proof produced");
  [%expect
    {|
    (check (result (Ok ())))
      s13: select(x, array_extensionality.<fresh>) = select(y, array_extensionality.<fresh>)   [∀-instantiation {k.bound.<fresh> := array_extensionality.<fresh>} over [s0]]
            r3: a = b ∨ select(a, array_extensionality.<fresh>) ≠ select(b, array_extensionality.<fresh>)   [array extensionality: a ≠ b ⟹ select(a, array_extensionality.<fresh>) ≠ select(b, array_extensionality.<fresh>)]
            r4: select(x, array_extensionality.<fresh>) = select(y, array_extensionality.<fresh>)   [s13]
            r5: x ≠ a ∨ y ≠ b ∨ select(x, array_extensionality.<fresh>) ≠ select(y, array_extensionality.<fresh>) ∨ select(a, array_extensionality.<fresh>) = select(b, array_extensionality.<fresh>)   [EUF: select(a, array_extensionality.<fresh>) = select(b, array_extensionality.<fresh>) via [x = a; congruence(select(a, array_extensionality.<fresh>) = select(x, array_extensionality.<fresh>) from [a = x, array_extensionality.<fresh> = array_extensionality.<fresh>]); select(x, array_extensionality.<fresh>) = select(y, array_extensionality.<fresh>); y = b; congruence(select(y, array_extensionality.<fresh>) = select(b, array_extensionality.<fresh>) from [y = b, array_extensionality.<fresh> = array_extensionality.<fresh>])]]
    |}]
;;

let%expect_test "bogus array certificates are rejected" =
  let array = v "arr" in
  let index = v "idx" in
  let witness = v "wit" in
  let left = v "left" in
  let right = v "right" in
  let row1_clause value =
    clause_exn
      [ theory_literal
          (`Eq
            (Formula.Select (Formula.Store (array, index, value), index), value))
          ~positive:true
      ]
  in
  let row1_certificate value : Proof.Theory_certificate.Array.t =
    Read_over_write_same_index { array; index; value }
  in
  let extensionality_certificate ~type_premises
    : Proof.Theory_certificate.Array.t
    =
    Extensionality { left; right; witness; type_premises }
  in
  let extensionality_clause ~type_premises ~certificate_left ~certificate_right =
    let not_has_type (var, type_expr) =
      theory_literal (`Type_eq (Type_expr.Var var, type_expr)) ~positive:false
    in
    clause_exn
      (List.map type_premises ~f:not_has_type
       @ [ theory_literal
             (`Eq (certificate_left, certificate_right))
             ~positive:true
         ; theory_literal
             (`Eq
               ( Formula.Select (certificate_left, witness)
               , Formula.Select (certificate_right, witness) ))
             ~positive:false
         ])
  in
  let int_array : Type_expr.t =
    Type_expr.Array_type (Type_expr.Base Int, Type_expr.Base Int)
  in
  let guarded_clause =
    extensionality_clause
      ~type_premises:[ Tvar.of_string "arr", int_array ]
      ~certificate_left:left
      ~certificate_right:right
  in
  let check clause certificate =
    Proof.check_theory_certificate ~clause (Array certificate)
  in
  print_s
    [%message
      "bogus array certificates"
        ~row1_wrong_value:
          (Or_error.is_error
             (check (row1_clause other) (row1_certificate value))
           : bool)
        ~row1_wrong_index:
          (Or_error.is_error
             (check (row1_clause value) (row1_certificate other))
           : bool)
        ~extensionality_missing_guard:
          (Or_error.is_error
             (check
                guarded_clause
                (extensionality_certificate ~type_premises:[]))
           : bool)
        ~extensionality_unearned_guard:
          (Or_error.is_error
             (check
                (extensionality_clause
                   ~type_premises:[]
                   ~certificate_left:left
                   ~certificate_right:right)
                (extensionality_certificate
                   ~type_premises:[ Tvar.of_string "arr", int_array ]))
           : bool)];
  print_s
    [%sexp
      (check
         (row1_clause other)
         (Read_over_write_same_index { array; index; value })
       : unit Or_error.t)];
  [%expect
    {|
    ("bogus array certificates" (row1_wrong_value true) (row1_wrong_index true)
     (extensionality_missing_guard true) (extensionality_unearned_guard true))
    (Error "array certificate does not match its clause")
    |}]
;;
