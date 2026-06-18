open! Core
open! Feel.Import

module Type : sig
  type t =
    | Int
    | Float
  [@@deriving sexp, compare, equal, hash]
end

module Atom : sig
  type t = [ `Has_type of Tvar.t * Type.t ] [@@deriving sexp, compare, hash]

  val normalize : t -> t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

type t

val create : unit -> t

(** Register a [Has_type] atom and its SAT variable. Must be called before the
    SAT solver sees any clause referencing [sat_var]. *)
val add_atom : t -> atom:Atom.t -> sat_var:int -> unit

(** The SAT variable for [(var, typ)], if that atom has been registered. *)
val sat_var_for : t -> Tvar.t -> Type.t -> int option

(** The current type of [var] as determined by the SAT solver's most recent
    propagation. Reliable between [solve] calls for globally-asserted types
    (decision level 0); may be stale for scoped assertions. *)
val get_type : t -> Tvar.t -> Type.t option

include Feel.Theory.S with type t := t
