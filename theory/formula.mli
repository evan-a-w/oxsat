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

(** Encoding state shared across multiple [encode] calls: tracks fresh
    variable allocation and the mapping from theory atoms to the SAT
    variables that represent them, so that the same atom always maps to the
    same variable. *)
module Encoding : sig
  type t

  val create : unit -> t

  (** Allocates and returns a fresh SAT variable not used by any atom or
      subformula encoding. *)
  val fresh_var : t -> int

  (** The SAT variable representing [atom], allocating a fresh one if
      [atom] (after normalization) hasn't been seen before. *)
  val sat_var_for_atom : t -> Atom.t -> int

  (** All [(atom, sat_var)] pairs registered so far, in the order in which
      they were first encountered, with each [atom] normalized. *)
  val atoms : t -> (Atom.t * int) list

  (** A position in the atom registration order, for use with
      [new_atoms_since]. *)
  val checkpoint : t -> int

  (** [(atom, sat_var)] pairs registered since [checkpoint]. *)
  val new_atoms_since : t -> checkpoint:int -> (Atom.t * int) list
end

(** [encode encoding formula] Tseitin-encodes [formula] into CNF, returning
    a list of clauses whose conjunction is satisfiable iff [formula] is, and
    such that any satisfying assignment of the clauses makes [formula] true.
    Fresh Tseitin variables and atom SAT variables are allocated from
    [encoding] as needed; reusing an [Encoding.t] across multiple calls keeps
    atom-to-variable assignments consistent. *)
val encode : Encoding.t -> t -> int array list
