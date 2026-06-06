open! Core

module type Arg = sig
  type t

  include Hashable.S with type t := t
end

module type S = sig
  type t

  module Arg : Arg

  val intern : Arg.t -> t
  val unintern : t -> Arg.t
end

module type Interned = sig
  module type S = S

  val make : (module Arg with type t = 'a) -> (module S with type Arg.t = 'a)

  module Global_string : S with module Arg := String
end
