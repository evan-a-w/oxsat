open! Core
open! Theory_core

(* Human-readable rendering of a proof: formulas as ordinary mathematical
   notation and the proof/refutation structure as an indented listing. Intended
   for reading and for expect tests, not for machine consumption. *)

let q_to_string q =
  let num = Q.num q
  and den = Q.den q in
  if den = 1 then Int.to_string num else sprintf "%d/%d" num den
;;

let rec type_expr_to_string (t : Type_expr.t) =
  match t with
  | Var v -> Tvar.to_string v
  | Base Bool -> "bool"
  | Base Int -> "int"
  | Base Float -> "float"
  | Type_of v -> sprintf "typeof(%s)" (Tvar.to_string v)
  | App (f, args) ->
    sprintf
      "%s(%s)"
      (Tvar.to_string f)
      (String.concat ~sep:", " (List.map args ~f:type_expr_to_string))
  | Function_type (a, b) ->
    sprintf "(%s -> %s)" (type_expr_to_string a) (type_expr_to_string b)
  | Type -> "type"
;;

let linear_expr_to_string ({ coeffs; const } : Linear_expr.t) =
  let terms =
    Map.to_alist coeffs
    |> List.map ~f:(fun (v, c) ->
      if Q.equal c Q.one
      then Tvar.to_string v
      else if Q.equal c (Q.neg Q.one)
      then sprintf "-%s" (Tvar.to_string v)
      else sprintf "%s%s" (q_to_string c) (Tvar.to_string v))
  in
  let terms =
    if Q.is_zero const then terms else terms @ [ q_to_string const ]
  in
  match terms with
  | [] -> "0"
  | _ -> String.concat ~sep:" + " terms
;;

(* Renders a formula as mathematical notation. Handled per outermost
   constructor; nested boolean structure is parenthesized. *)
let rec formula_to_string (formula : Formula.any) =
  match formula with
  | Var v -> Tvar.to_string v
  | Eq (a, b) -> sprintf "%s = %s" (formula_to_string a) (formula_to_string b)
  | True -> "true"
  | False -> "false"
  | Not (Eq (a, b)) ->
    sprintf "%s ≠ %s" (formula_to_string a) (formula_to_string b)
  | Not a -> sprintf "¬%s" (atom_to_string a)
  | And [] -> "true"
  | Or [] -> "false"
  | And fs -> String.concat ~sep:" ∧ " (List.map fs ~f:atom_to_string)
  | Or fs -> String.concat ~sep:" ∨ " (List.map fs ~f:atom_to_string)
  | App (f, args) ->
    sprintf
      "%s(%s)"
      (Tvar.to_string f)
      (String.concat ~sep:", " (List.map args ~f:formula_to_string))
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
  | Type -> "type"
  | Function_type (a, b) ->
    sprintf "(%s -> %s)" (formula_to_string a) (formula_to_string b)
  | Type_of a -> sprintf "typeof(%s)" (formula_to_string a)
  | Type_var v -> Tvar.to_string v
  | Type_app (f, args) ->
    sprintf
      "%s(%s)"
      (Tvar.to_string f)
      (String.concat ~sep:", " (List.map args ~f:formula_to_string))
  | La_const q -> q_to_string q
  | La_scale_const (q, a) ->
    if Q.equal q Q.one
    then formula_to_string a
    else sprintf "%s*%s" (q_to_string q) (formula_to_string a)
  | La_add (a, b) ->
    sprintf "%s + %s" (formula_to_string a) (formula_to_string b)
  | La_compare (a, op, b) ->
    let op =
      match op with
      | `Le -> "≤"
      | `Ge -> "≥"
      | `Lt -> "<"
      | `Gt -> ">"
    in
    sprintf "%s %s %s" (formula_to_string a) op (formula_to_string b)

(* A sub-formula that appears as a conjunct/disjunct/negand: parenthesize
   compound boolean structure so precedence is unambiguous. *)
and atom_to_string (formula : Formula.any) =
  match formula with
  | And (_ :: _ :: _) | Or (_ :: _ :: _) ->
    sprintf "(%s)" (formula_to_string formula)
  | _ -> formula_to_string formula
;;

let theory_atom_to_string (atom : Atom.t) =
  match atom with
  | `Eq (a, b) -> sprintf "%s = %s" (formula_to_string a) (formula_to_string b)
  | `Type_eq (a, b) ->
    sprintf "%s = %s" (type_expr_to_string a) (type_expr_to_string b)
  | `Le (e, c) -> sprintf "%s ≤ %s" (linear_expr_to_string e) (q_to_string c)
;;

let proof_atom_to_string (atom : Proof_atom.t) =
  match atom with
  | Theory atom -> theory_atom_to_string atom
  | Extension id -> sprintf "e%d" (Proof_id.Extension.to_int id)
;;

let literal_to_string (literal : Proof_literal.t) =
  if literal.positive
  then proof_atom_to_string literal.atom
  else sprintf "¬(%s)" (proof_atom_to_string literal.atom)
;;

let clause_to_string clause =
  match Proof_clause.literals clause with
  | [||] -> "⊥"
  | literals ->
    String.concat
      ~sep:" ∨ "
      (Array.to_list literals |> List.map ~f:literal_to_string)
;;

let rec boolean_to_string (b : Proof_boolean.t) =
  match b with
  | True -> "true"
  | False -> "false"
  | Atom atom -> proof_atom_to_string atom
  | Not b -> sprintf "¬(%s)" (boolean_to_string b)
  | And bs ->
    sprintf "(%s)" (String.concat ~sep:" ∧ " (List.map bs ~f:boolean_to_string))
  | Or bs ->
    sprintf "(%s)" (String.concat ~sep:" ∨ " (List.map bs ~f:boolean_to_string))
;;

module Buffer_out = struct
  type t =
    { buffer : Buffer.t
    ; mutable indent : int
    }

  let create () = { buffer = Buffer.create 256; indent = 0 }
  let line t s = bprintf t.buffer "%s%s\n" (String.make t.indent ' ') s

  let indented t ~f =
    t.indent <- t.indent + 2;
    f ();
    t.indent <- t.indent - 2
  ;;

  let contents t = Buffer.contents t.buffer
end

let euf_equality_to_string
  ({ left; right } : Proof_theory_certificate.Euf.Equality.t)
  =
  sprintf "%s = %s" (formula_to_string left) (formula_to_string right)
;;

let euf_justification_to_string
  (j : Proof_theory_certificate.Euf.Justification.t)
  =
  match j with
  | Asserted { clause_literal } -> sprintf "asserted(lit %d)" clause_literal
  | Congruence { left; right; argument_equalities } ->
    sprintf
      "congruence(%s = %s from [%s])"
      (formula_to_string left)
      (formula_to_string right)
      (String.concat
         ~sep:", "
         (List.map argument_equalities ~f:euf_equality_to_string))
;;

let euf_equality_proof_to_string
  ({ conclusion; path } : Proof_theory_certificate.Euf.Equality_proof.t)
  =
  sprintf
    "%s via [%s]"
    (euf_equality_to_string conclusion)
    (String.concat ~sep:"; " (List.map path ~f:euf_justification_to_string))
;;

let certificate_to_string (certificate : Proof_theory_certificate.t) =
  match certificate with
  | Euf (Equality proof) ->
    sprintf "EUF equality: %s" (euf_equality_proof_to_string proof)
  | Euf
      (Disequality { conclusion; asserted_disequality; left_path; right_path })
    ->
    sprintf
      "EUF disequality: %s ≠, from asserted eq lit %d, left [%s], right [%s]"
      (euf_equality_to_string conclusion)
      asserted_disequality
      (euf_equality_proof_to_string left_path)
      (euf_equality_proof_to_string right_path)
  | Linear_arithmetic { combination } ->
    sprintf
      "Farkas: %s"
      (String.concat
         ~sep:" + "
         (List.map combination ~f:(fun { clause_literal; coefficient } ->
            sprintf "%s*lit %d" (q_to_string coefficient) clause_literal)))
  | Integer_split { variable; floor; ceil } ->
    sprintf
      "integer split: %s ≤ %s ∨ %s ≥ %s"
      (Tvar.to_string variable)
      (q_to_string floor)
      (Tvar.to_string variable)
      (q_to_string ceil)
  | Type_theory { left; right; premise_literals } ->
    sprintf
      "type clash: %s vs %s, from lits [%s]"
      (type_expr_to_string left)
      (type_expr_to_string right)
      (String.concat ~sep:", " (List.map premise_literals ~f:Int.to_string))
  | Bare_var_eq certificate ->
    (match certificate with
     | Equality_implies_type_equality (a, b) ->
       sprintf "%s = %s ⟹ types equal" (Tvar.to_string a) (Tvar.to_string b)
     | Equality_implies_le { left; right; direction } ->
       let l, r =
         match direction with
         | Left_le_right -> left, right
         | Right_le_left -> right, left
       in
       sprintf
         "%s = %s ⟹ %s ≤ %s"
         (Tvar.to_string left)
         (Tvar.to_string right)
         (Tvar.to_string l)
         (Tvar.to_string r)
     | Numeric_coincidence_implies_equality (a, b) ->
       sprintf
         "%s ≤ %s ∧ %s ≤ %s ⟹ %s = %s"
         (Tvar.to_string a)
         (Tvar.to_string b)
         (Tvar.to_string b)
         (Tvar.to_string a)
         (Tvar.to_string a)
         (Tvar.to_string b))
;;

let reason_to_string (reason : Refutation.Reason.t) =
  match reason with
  | Input_clause { input; literal = _ } -> sprintf "input assumption %d" input
  | Extension_definition id ->
    sprintf "definition of e%d" (Proof_id.Extension.to_int id)
  | Theory_lemma certificate -> certificate_to_string certificate
  | Rup { hints } ->
    sprintf
      "RUP over [%s]"
      (String.concat
         ~sep:", "
         (Array.to_list hints
          |> List.map ~f:(fun h ->
            sprintf "r%d" (Proof_id.Refutation_step.to_int h))))
;;

(* Renders a refutation into [out] at the current indent. Kept here (rather than
   in [Proof]) since it needs only the [Refutation] and certificate modules. *)
let render_refutation out (refutation : Refutation.t) =
  let open Buffer_out in
  line out "refutation:";
  indented out ~f:(fun () ->
    if not (Array.is_empty refutation.extensions)
    then (
      line out "extensions:";
      indented out ~f:(fun () ->
        Array.iter refutation.extensions ~f:(fun extension ->
          line
            out
            (sprintf
               "e%d := %s"
               (Proof_id.Extension.to_int extension.id)
               (boolean_to_string extension.definition)))));
    line out "steps:";
    indented out ~f:(fun () ->
      Array.iteri refutation.steps ~f:(fun index step ->
        line
          out
          (sprintf
             "r%d: %s   [%s]"
             index
             (clause_to_string step.Refutation.Step.clause)
             (reason_to_string step.reason)))))
;;
