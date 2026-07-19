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
  (* [Var v] here is the type of value [v]; [typeof(v)] disambiguates it from a
     concrete type name. In a typing judgement it is instead consumed and
     printed as [v : τ]. *)
  | Var v -> sprintf "typeof(%s)" (Tvar.to_string v)
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

let is_concrete_type (formula : Formula.any) =
  match formula with
  | Bool | Int | Float | Type | Function_type _ | Type_app _ -> true
  | _ -> false
;;

(* Renders a formula as mathematical notation. Handled per outermost
   constructor; nested boolean structure is parenthesized. *)
let rec formula_to_string (formula : Formula.any) =
  match formula with
  | Var v -> Tvar.to_string v
  | Eq (a, b) ->
    (match typing_judgement a b with
     | Some judgement -> judgement
     | None -> sprintf "%s = %s" (formula_to_string a) (formula_to_string b))
  | True -> "true"
  | False -> "false"
  | Not (Eq (a, b)) ->
    (* A negated typing judgement reads as [¬(v : τ)]; a plain disequality as
       [a ≠ b]. *)
    (match typing_judgement a b with
     | Some judgement -> sprintf "¬(%s)" judgement
     | None -> sprintf "%s ≠ %s" (formula_to_string a) (formula_to_string b))
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
  (* A bare [Type_var v] is the *type of* value [v]; render as [typeof(v)] to
     distinguish it from a UF-role [Var v] (which prints as just [v]). A
     [Type_var] paired with a concrete type in an equality is instead consumed
     by [typing_judgement] and prints as [v : τ]. *)
  | Type_var v -> sprintf "typeof(%s)" (Tvar.to_string v)
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

(* If [a = b] equates the type of a value ([Type_var v]/[Type_of v]) with a
   concrete type, renders it as the typing judgement [v : τ]; otherwise [None]
   (so it prints as an ordinary equality). *)
and typing_judgement (a : Formula.any) (b : Formula.any) =
  let value_type formula =
    match (formula : Formula.any) with
    | Type_var v -> Some (Tvar.to_string v)
    | Type_of v -> Some (formula_to_string v)
    | _ -> None
  in
  match value_type a, value_type b with
  | Some v, None when is_concrete_type b ->
    Some (sprintf "%s : %s" v (formula_to_string b))
  | None, Some v when is_concrete_type a ->
    Some (sprintf "%s : %s" v (formula_to_string a))
  | _ -> None
;;

(* Typing-judgement form for a [`Type_eq] whose sides are a value's type
   ([Type_expr.Var]) and a concrete type; else [None]. *)
let type_expr_judgement (a : Type_expr.t) (b : Type_expr.t) =
  let concrete (t : Type_expr.t) =
    match t with
    | Var _ | Type_of _ -> false
    | Base _ | App _ | Function_type _ | Type -> true
  in
  match a, b with
  | Var v, _ when concrete b ->
    Some (sprintf "%s : %s" (Tvar.to_string v) (type_expr_to_string b))
  | _, Var v when concrete a ->
    Some (sprintf "%s : %s" (Tvar.to_string v) (type_expr_to_string a))
  | _ -> None
;;

let theory_atom_to_string (atom : Atom.t) =
  match atom with
  | `Eq (a, b) ->
    (match typing_judgement a b with
     | Some judgement -> judgement
     | None -> sprintf "%s = %s" (formula_to_string a) (formula_to_string b))
  | `Type_eq (a, b) ->
    (match type_expr_judgement a b with
     | Some judgement -> judgement
     | None -> sprintf "%s = %s" (type_expr_to_string a) (type_expr_to_string b))
  | `Le (e, c) -> sprintf "%s ≤ %s" (linear_expr_to_string e) (q_to_string c)
;;

let proof_atom_to_string (atom : Proof_atom.t) =
  match atom with
  | Theory atom -> theory_atom_to_string atom
  | Extension id -> sprintf "e%d" (Proof_id.Extension.to_int id)
;;

(* Renders a negated equality/type-equality atom with [≠] (or a negated typing
   judgement), matching how formulas print; other negated atoms get [¬(...)]. *)
let negated_atom_to_string (atom : Proof_atom.t) =
  match atom with
  | Theory (`Eq (a, b)) ->
    (match typing_judgement a b with
     | Some judgement -> sprintf "¬(%s)" judgement
     | None -> sprintf "%s ≠ %s" (formula_to_string a) (formula_to_string b))
  | Theory (`Type_eq (a, b)) ->
    (match type_expr_judgement a b with
     | Some judgement -> sprintf "¬(%s)" judgement
     | None -> sprintf "%s ≠ %s" (type_expr_to_string a) (type_expr_to_string b))
  | Theory (`Le _) | Extension _ -> sprintf "¬(%s)" (proof_atom_to_string atom)
;;

let literal_to_string (literal : Proof_literal.t) =
  if literal.positive
  then proof_atom_to_string literal.atom
  else negated_atom_to_string literal.atom
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

let clause_literal ~clause index =
  let literals = Proof_clause.literals clause in
  if index < 0 || index >= Array.length literals
  then None
  else Some literals.(index)
;;

(* Renders the atom of clause literal [index], ignoring its polarity. Used where
   a certificate cites a literal purely to name the underlying atom (EUF
   asserted edges and disequality premises cite negative/positive equality
   literals whose *atom* is the assumed equality either way). *)
let cited_atom ~clause index =
  match clause_literal ~clause index with
  | None -> sprintf "<lit %d?>" index
  | Some literal -> proof_atom_to_string literal.Proof_literal.atom
;;

(* Renders the fact a Farkas term assumes at clause literal [index]: the atom
   when the literal is negative in the clause, its negation when positive (the
   checker takes [assumed_value = not positive]). *)
let cited_assumed ~clause index =
  match clause_literal ~clause index with
  | None -> sprintf "<lit %d?>" index
  | Some literal ->
    let atom = proof_atom_to_string literal.Proof_literal.atom in
    if literal.positive then sprintf "¬(%s)" atom else atom
;;

let euf_justification_to_string
  ~clause
  (j : Proof_theory_certificate.Euf.Justification.t)
  =
  match j with
  | Asserted { clause_literal } ->
    sprintf "%s" (cited_atom ~clause clause_literal)
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
  ~clause
  ({ conclusion; path } : Proof_theory_certificate.Euf.Equality_proof.t)
  =
  sprintf
    "%s via [%s]"
    (euf_equality_to_string conclusion)
    (String.concat
       ~sep:"; "
       (List.map path ~f:(euf_justification_to_string ~clause)))
;;

let certificate_to_string ~clause (certificate : Proof_theory_certificate.t) =
  match certificate with
  | Euf (Equality proof) ->
    sprintf "EUF: %s" (euf_equality_proof_to_string ~clause proof)
  | Euf
      (Disequality { conclusion; asserted_disequality; left_path; right_path })
    ->
    sprintf
      "EUF: %s is false, given assumed %s, via left [%s], right [%s]"
      (euf_equality_to_string conclusion)
      (cited_atom ~clause asserted_disequality)
      (euf_equality_proof_to_string ~clause left_path)
      (euf_equality_proof_to_string ~clause right_path)
  | Linear_arithmetic { combination } ->
    sprintf
      "Farkas: %s ⟹ false"
      (String.concat
         ~sep:" + "
         (List.map combination ~f:(fun { clause_literal; coefficient } ->
            let assumed = cited_assumed ~clause clause_literal in
            if Q.equal coefficient Q.one
            then sprintf "(%s)" assumed
            else sprintf "%s·(%s)" (q_to_string coefficient) assumed)))
  | Integer_split { variable; floor; ceil } ->
    sprintf
      "integer split: %s ≤ %s ∨ %s ≥ %s"
      (Tvar.to_string variable)
      (q_to_string floor)
      (Tvar.to_string variable)
      (q_to_string ceil)
  | Type_theory { left; right; premise_literals } ->
    sprintf
      "type clash: %s vs %s, given [%s]"
      (type_expr_to_string left)
      (type_expr_to_string right)
      (String.concat
         ~sep:", "
         (List.map premise_literals ~f:(cited_atom ~clause)))
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

let reason_to_string ~clause ~num_assumptions (reason : Refutation.Reason.t) =
  match reason with
  | Input_clause { input; literal = _ } ->
    (* [input] indexes the refutation's inputs = the proof's assumptions
       followed by a trailing [¬false]. Within the assumption range it *is*
       assumption a[input]; the sentinel (which a real refutation never cites) is
       named as a bare input to avoid implying a nonexistent assumption. *)
    if input < num_assumptions
    then sprintf "assumption a%d" input
    else sprintf "input i%d (¬false)" input
  | Extension_definition id ->
    sprintf "definition of e%d" (Proof_id.Extension.to_int id)
  | Theory_lemma certificate -> certificate_to_string ~clause certificate
  | Rup { hints } ->
    sprintf
      "RUP over [%s]"
      (String.concat
         ~sep:", "
         (Array.to_list hints
          |> List.map ~f:(fun h ->
            sprintf "r%d" (Proof_id.Refutation_step.to_int h))))
;;

(* Renders a refutation into [out] at the current indent. [num_assumptions] is
   the enclosing proof's assumption count, used to name input references. Kept
   here (rather than in [Proof]) since it needs only the [Refutation] and
   certificate modules. *)
let render_refutation out ~num_assumptions (refutation : Refutation.t) =
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
             (reason_to_string
                ~clause:step.Refutation.Step.clause
                ~num_assumptions
                step.reason)))))
;;
