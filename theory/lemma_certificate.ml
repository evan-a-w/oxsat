open! Core
open! Import

(* A theory lemma's proof certificate expressed over the lemma's atoms rather
   than over positional clause-literal indices. [Proof_generation] resolves each
   referenced atom to its index in the final proof clause (see
   [to_theory_certificate]). This lets each sub-theory describe its certificate
   without knowing how [lemma_to_clause] and [Proof.Clause.create] will order
   the literals. *)

module Euf = struct
  module Justification = struct
    type t =
      | Asserted of { disequality : Atom.t }
      | Congruence of
          { left : Formula.any
          ; right : Formula.any
          ; argument_equalities : Proof.Theory_certificate.Euf.Equality.t list
          }
  end

  module Equality_proof = struct
    type t =
      { conclusion : Proof.Theory_certificate.Euf.Equality.t
      ; path : Justification.t list
      }
  end

  type t =
    | Equality of Equality_proof.t
    | Disequality of
        { conclusion : Proof.Theory_certificate.Euf.Equality.t
        ; asserted_disequality : Atom.t
        ; left_path : Equality_proof.t
        ; right_path : Equality_proof.t
        }
end

module Linear_arithmetic = struct
  (* Farkas coefficient on each cited [`Le] atom. *)
  type t = { combination : (Atom.t * Q.t) list }
end

module Type_theory = struct
  type t =
    { left : Type_expr.t
    ; right : Type_expr.t
    ; premises : Atom.t list
    }
end

type t =
  | Euf of Euf.t
  | Linear_arithmetic of Linear_arithmetic.t
  | Integer_split of Proof.Theory_certificate.Integer_split.t
  | Type_theory of Type_theory.t
  | Bare_var_eq of Proof.Theory_certificate.Bare_var_eq.t

(* Index of the clause literal whose theory atom is [atom] (up to
   normalization), ignoring polarity. Raises if the atom is absent -- a producer
   bug, since every atom a certificate cites must appear in the lemma clause. *)
let index_of_atom clause (atom : Atom.t) =
  let normalized = Atom.normalize atom in
  let literals = Proof.Clause.literals clause in
  match
    Array.findi literals ~f:(fun _ (literal : Proof.Literal.t) ->
      match literal.atom with
      | Theory theory_atom ->
        [%compare.equal: Atom.t] (Atom.normalize theory_atom) normalized
      | Extension _ -> false)
  with
  | Some (index, _) -> index
  | None ->
    raise_s
      [%message
        "lemma certificate cites an atom absent from its clause" (atom : Atom.t)]
;;

let euf_justification clause (justification : Euf.Justification.t)
  : Proof.Theory_certificate.Euf.Justification.t
  =
  match justification with
  | Asserted { disequality } ->
    Asserted { clause_literal = index_of_atom clause disequality }
  | Congruence { left; right; argument_equalities } ->
    Congruence { left; right; argument_equalities }
;;

let euf_equality_proof clause ({ conclusion; path } : Euf.Equality_proof.t)
  : Proof.Theory_certificate.Euf.Equality_proof.t
  =
  { conclusion; path = List.map path ~f:(euf_justification clause) }
;;

let euf clause (certificate : Euf.t) : Proof.Theory_certificate.Euf.t =
  match certificate with
  | Equality proof -> Equality (euf_equality_proof clause proof)
  | Disequality { conclusion; asserted_disequality; left_path; right_path } ->
    Disequality
      { conclusion
      ; asserted_disequality = index_of_atom clause asserted_disequality
      ; left_path = euf_equality_proof clause left_path
      ; right_path = euf_equality_proof clause right_path
      }
;;

(* Resolves a lemma certificate's atom references to positional indices in
   [clause], producing the solver-independent certificate the refutation checker
   validates. *)
let to_theory_certificate clause (t : t) : Proof.Theory_certificate.t =
  match t with
  | Euf certificate -> Euf (euf clause certificate)
  | Linear_arithmetic { combination } ->
    Linear_arithmetic
      { combination =
          List.map combination ~f:(fun (atom, coefficient) ->
            { Proof.Theory_certificate.Linear_arithmetic.Term.clause_literal =
                index_of_atom clause atom
            ; coefficient
            })
      }
  | Integer_split certificate -> Integer_split certificate
  | Type_theory { left; right; premises } ->
    Type_theory
      { left
      ; right
      ; premise_literals = List.map premises ~f:(index_of_atom clause)
      }
  | Bare_var_eq certificate -> Bare_var_eq certificate
;;
