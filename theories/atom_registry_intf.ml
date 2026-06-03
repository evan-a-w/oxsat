open! Core

module type Atom = sig
  type t

  val create_unregistered : unit -> t option

  include Hashable.S with type t := t
  include Comparable.S with type t := t
end

module type Data = sig
  type t

  val create_unregistered : unit -> t
end

module type S = sig
  type t
  type atom
  type data

  val create : unit -> t
  val register : t -> atom:atom -> data:data -> int
  val on_new_var : t -> int -> unit
  val var : t -> atom:atom -> int or_null
  val atom : t -> var:int -> atom or_null
  val data : t -> atom:atom -> data option
end

module type Atom_registry = sig
  module type S = S

  module Make (Atom : Atom) (Data : Data) :
    S with type atom := Atom.t and type data := Data.t
end
