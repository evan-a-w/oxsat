open! Core
open! Feel.Import

module type Term = sig
  type t [@@deriving sexp, compare, hash]

  val split_function : t -> (Tvar.t * t list) option

  (** for vec *)
  val garbage_for_vec : t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

(** Decision procedure for the theory of equality over uninterpreted functions
    (EUF / "congruence closure"), implementing {!Theory.S}. *)
module type S = sig
  module Term : Term

  module Atom : sig
    type t = [ `Eq of Term.t * Term.t ] [@@deriving sexp, compare, hash]

    val normalize : t -> t

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end

  type t

  val create : atoms:Atom.t list -> t
  val add_atom : t -> atom:Atom.t -> unit
  val assert_atom : t -> decision_level:int -> atom:Atom.t -> value:bool -> unit
  val maybe_get_lemma : t -> [ `Consistent | `Lemma of (Atom.t * bool) list ]
  val undo : t -> to_decision_level_excl:int -> unit
end

module type Uninterpreted_functions = sig
  module Term : sig
    type t =
      [ `App of function_:Tvar.t * args:t list
      | `Var of Tvar.t
      ]
    [@@deriving sexp, compare, hash]

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end

  module Make (Term : Term) : S with module Term := Term
end
