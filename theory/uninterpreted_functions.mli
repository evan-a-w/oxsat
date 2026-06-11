open! Core
open! Feel.Import

(** Decision procedure for the theory of equality over uninterpreted functions
    (EUF / "congruence closure"), implementing {!Theory.S}. *)

module Atom : sig
  type t = [ `Eq of Term.t * Term.t ] [@@deriving sexp, compare, hash]

  val normalize : t -> t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Term : sig
  type t =
    [ `App of function_:Tvar.t * args:t list
    | `Var of Tvar.t
    ]
  [@@deriving sexp, compare, hash]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

type t

(** [create ~atoms] builds the theory state for the given set of equality atoms,
    each paired with the SAT variable that represents it. All terms appearing in
    [atoms] are registered up front; the theory does not introduce new terms or
    atoms afterwards. *)
val create : atoms:(Atom.t * int) list -> t

(** Registers an additional equality atom (and its terms) into an existing
    theory instance. Must be called before [sat_var] (or any clause referencing
    it) is given to the SAT solver, so that [assert_literal] can recognize
    [sat_var] as a theory atom from the start. Registration is not part of the
    undo trail and is never undone by [undo] -- the new term and atom remain
    known to the theory even if the solver later backtracks past this point. *)
val add_atom : t -> atom:Atom.t -> sat_var:int -> unit

include Feel.Theory.S with type t := t
