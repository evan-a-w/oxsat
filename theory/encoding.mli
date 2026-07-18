open! Core
open! Feel.Import

module Formula_with_no_shared_theories : sig
  type t =
    | True
    | False
    | Atom of Atom.t
    | Not of t
    | And of t list
    | Or of t list
  [@@deriving sexp_of]
end

type t

val create : unit -> t
val fresh_var : t -> int
val sat_var_for_atom : t -> Atom.t -> int
val find_sat_var_for_atom : t -> Atom.t -> int option
val atom_for_sat_var : t -> int -> Atom.t option
val atoms : t -> (Atom.t * int) list

(** Theory membership of a tvar, accumulated as atoms are created: a tvar seen
    in atoms of two different theories maps to [Shared]. Bare
    [`Eq (Var _, Var _)] atoms record nothing — they are the theory-agnostic
    bridge equalities whose cross-theory consequences [Bare_var_eq] injects
    lazily, gated on this map. *)
val theory_for_tvar : t -> Tvar.t -> Formula.Theory.Packed.t option

val tvar_theories : t -> Formula.Theory.Packed.t Tvar.Map.t

(** [encode encoding formula] elaborates the rich [formula] into flat theory
    atoms, then Tseitin-encodes the result into CNF, returning a list of clauses
    whose conjunction is satisfiable iff [formula] is, and such that any
    satisfying assignment of the clauses makes [formula] true. Fresh Tseitin
    variables and atom SAT variables are allocated from [encoding] as needed;
    reusing an [Encoding.t] across multiple calls keeps atom-to-variable
    assignments consistent.

    Returns an [Error] if [formula] is ill-formed, e.g. an [Eq] or [La_compare]
    whose arguments don't agree on which theory they belong to, or a [Type_of]
    applied to a non-variable term. *)
val encode : t -> formula:[> `Boolean ] Formula.t -> int array list Or_error.t

(** Reconstructs a [Formula.t] representing [atom], for surfacing atoms (e.g.
    unsat-core reasons) back as formulas. *)
val atom_to_formula : Atom.t -> Formula.any

(** Reconstructs a [Formula.t] representing [le], e.g. for building [La_compare]
    formulas out of a [Linear_expr.t] built via arithmetic on [Tvar.t]s. *)
val linear_expr_to_formula : Linear_expr.t -> [> `La | `Term ] Formula.t

(** Reconstructs a [Formula.t] representing [type_expr], widening
    [Type_expr.Var] to [Formula.Type_var] so it's distinguishable from a bare
    [Formula.Var] used as a UF term. *)
val type_expr_to_formula : Type_expr.t -> [> `Type | `Term ] Formula.t

(** Interprets [formula] as a type expression; the inverse of
    {!type_expr_to_formula}. *)
val type_expr_of : [> `Type | `Term ] Formula.t -> Type_expr.t Or_error.t
