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

val create : atoms:Atom.t list -> t
val add_atom : t -> atom:Atom.t -> unit
val assert_atom : t -> decision_level:int -> atom:Atom.t -> value:bool -> unit
val maybe_get_lemma : t -> [ `Consistent | `Lemma of (Atom.t * bool) list ]
val undo : t -> to_decision_level_excl:int -> unit
