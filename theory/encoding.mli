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

module Checkpoint : sig
  type t
end

val create : unit -> t
val fresh_var : t -> int
val sat_var_for_atom : t -> Atom.t -> int
val find_sat_var_for_atom : t -> Atom.t -> int option
val atom_for_sat_var : t -> int -> Atom.t option
val atoms : t -> (Atom.t * int) list
val checkpoint : t -> Checkpoint.t
val new_atoms_since : t -> checkpoint:Checkpoint.t -> (Atom.t * int) list
val new_shared_tvars_since : t -> checkpoint:Checkpoint.t -> Tvar.t list

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
