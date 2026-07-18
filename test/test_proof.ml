open! Core
open! Feel.Import
open! Theory_core
open! Theory

let x = Tvar.of_string "x"
let y = Tvar.of_string "y"

let equality_literal ~positive =
  Proof.Literal.create
    ~atom:(Proof.Atom.Theory (`Eq (Formula.Var y, Formula.Var x)))
    ~positive
;;

let print_clause = function
  | `Tautology -> print_endline "Tautology"
  | `Clause clause -> print_s [%sexp (clause : Proof.Clause.t)]
;;

let%expect_test "proof clauses normalize, sort, and deduplicate literals" =
  let extension =
    Proof.Literal.create
      ~atom:(Proof.Atom.Extension (Proof.Id.Extension.of_int_exn 0))
      ~positive:false
  in
  let clause =
    Proof.Clause.create
      [ equality_literal ~positive:true
      ; extension
      ; equality_literal ~positive:true
      ]
  in
  print_clause clause;
  (match clause with
   | `Tautology -> assert false
   | `Clause clause ->
     let sexp = Proof.Clause.sexp_of_t clause in
     print_s
       [%message
         "round trip"
           ~equal:
             (Proof.Clause.compare clause (Proof.Clause.t_of_sexp sexp) = 0
              : bool)]);
  print_clause
    (Proof.Clause.create
       [ equality_literal ~positive:true; equality_literal ~positive:false ]);
  [%expect
    {|
    (((atom (Theory (Eq ((Var x) (Var y))))) (positive true))
     ((atom (Extension 0)) (positive false)))
    ("round trip" (equal true))
    Tautology
    |}]
;;

let%expect_test "manual proofs have a stable S-expression representation" =
  let formula : Formula.any = Eq (Var x, Var x) in
  let proof : Proof.t =
    { assumptions = [| { name = Some "h"; formula } |]
    ; steps =
        [| { name = Some "same"
           ; conclusion = formula
           ; justification = Assumption (Proof.Id.Assumption.of_int_exn 0)
           }
        |]
    ; conclusion = Proof.Id.Step.of_int_exn 0
    }
  in
  let sexp = Proof.sexp_of_t proof in
  print_s sexp;
  print_s
    [%message
      "round trip"
        ~equal:(Proof.compare proof (Proof.t_of_sexp sexp) = 0 : bool)];
  [%expect
    {|
    ((assumptions (((name (h)) (formula (Eq (Var x) (Var x))))))
     (steps
      (((name (same)) (conclusion (Eq (Var x) (Var x)))
        (justification (Assumption 0)))))
     (conclusion 0))
    ("round trip" (equal true))
    |}]
;;
