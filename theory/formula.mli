open! Core
open! Feel.Import

(** Propositional formulas over theory atoms. *)
type t =
  | True
  | False
  | Atom of Atom.t
  | Not of t
  | And of t list
  | Or of t list
[@@deriving sexp]

(** Encoding state shared across multiple [encode] calls: tracks fresh variable
    allocation and the mapping from theory atoms to the SAT variables that
    represent them, so that the same atom always maps to the same variable. *)
module Encoding : sig
  type t

  val create : unit -> t
  val fresh_var : t -> int
  val sat_var_for_atom : t -> Atom.t -> int
  val find_sat_var_for_atom : t -> Atom.t -> int option
  val atom_for_sat_var : t -> int -> Atom.t option

  (** Registers [atom] as an additional, independent theory atom for [sat_var],
      alongside whatever atom [sat_var_for_atom]/[atom_for_sat_var] already
      associate with it. Needed when a derived atom of a different shape (e.g.
      the [Eq] atom a [Type_eq] atom translates to for the EUF theory) must
      share a SAT variable with the atom that originally allocated it, while
      [atom_for_sat_var] keeps resolving to the original (e.g. for unsat
      cores). *)
  val set_theory_atom : t -> sat_var:int -> atom:Atom.t -> unit

  val theory_atom_for_sat_var : t -> int -> Atom.t option

  (** Raises if [atom] was never registered via [set_theory_atom]. *)
  val sat_var_for_theory_atom : t -> Atom.t -> int

  val atoms : t -> (Atom.t * int) list
  val checkpoint : t -> int
  val new_atoms_since : t -> checkpoint:int -> (Atom.t * int) list
end

(** [encode encoding formula] Tseitin-encodes [formula] into CNF, returning a
    list of clauses whose conjunction is satisfiable iff [formula] is, and such
    that any satisfying assignment of the clauses makes [formula] true. Fresh
    Tseitin variables and atom SAT variables are allocated from [encoding] as
    needed; reusing an [Encoding.t] across multiple calls keeps atom-to-variable
    assignments consistent. *)
val encode : Encoding.t -> t -> int array list
