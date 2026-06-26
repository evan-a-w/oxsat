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

(** [create ~atoms] builds the theory state for the given set of equality atoms.
    All terms appearing in [atoms] are registered up front; the theory does not
    introduce new terms or atoms afterwards. *)
val create : atoms:Atom.t list -> t

(** Registers an additional equality atom (and its terms) into an existing
    theory instance. Registration is not part of the undo trail and is never
    undone by [undo] -- the new term and atom remain known to the theory even if
    the solver later backtracks past this point. *)
val add_atom : t -> atom:Atom.t -> unit

(** Informs the theory of the current truth value of [atom], as determined by
    the SAT solver. Like {!Feel.Theory.S.assert_literal}, but in terms of an
    atom rather than a SAT literal -- the caller (typically [theory/solver.ml])
    is responsible for mapping SAT literals to atoms. *)
val assert_atom : t -> decision_level:int -> atom:Atom.t -> value:bool -> unit

(** Like {!Feel.Theory.S.maybe_get_lemma}, but the lemma is expressed as a list
    of literals over atoms rather than SAT literals -- the caller is responsible
    for mapping atoms to SAT variables (allocating fresh ones for atoms that
    don't have one yet). Each [(atom, polarity)] pair is a literal:
    [(atom, true)] is the positive literal, [(atom, false)] its negation. *)
val maybe_get_lemma : t -> [ `Consistent | `Lemma of (Atom.t * bool) list ]

val undo : t -> to_decision_level_excl:int -> unit
