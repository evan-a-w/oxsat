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

module Encoding : sig
  type t

  val create : unit -> t
  val fresh_var : t -> int
  val sat_var_for_atom : t -> Atom.t -> int
  val find_sat_var_for_atom : t -> Atom.t -> int option
  val atom_for_sat_var : t -> int -> Atom.t option
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
