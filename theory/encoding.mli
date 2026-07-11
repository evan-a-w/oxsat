open! Core
open! Feel.Import

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

(** [encode encoding formula] Tseitin-encodes [formula] into CNF, returning a
    list of clauses whose conjunction is satisfiable iff [formula] is, and such
    that any satisfying assignment of the clauses makes [formula] true. Fresh
    Tseitin variables and atom SAT variables are allocated from [encoding] as
    needed; reusing an [Encoding.t] across multiple calls keeps atom-to-variable
    assignments consistent. *)
val encode : t -> formula:Formula.t -> int array list
