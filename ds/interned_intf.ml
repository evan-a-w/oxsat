open! Core

module type Arg = sig
  type t [@@deriving sexp]

  include Hashable.S with type t := t
end

module type S = sig
  type t = private int [@@deriving sexp, equal, hash, compare]

  module Arg : Arg

  val intern : Arg.t -> t
  val unintern : t -> Arg.t
  val unsafe_clear_all : unit -> unit

  include Hashable.S with type t := t
  include Comparable.S with type t := t
end

module type Interned = sig
  module type S = S

  val make : (module Arg with type t = 'a) -> (module S with type Arg.t = 'a)

  module Global_string : S with type Arg.t = string
end
