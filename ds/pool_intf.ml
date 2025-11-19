open! Core
open! Unboxed

module type%template
  [@kind
    k
    = ( value
      , bits64
      , bits64 & bits64
      , bits64 & bits64 & bits64
      , value & value & value & value & value & value
      , value & value & bits64 & bits64 & bits64 & value
      , value & bits64 & bits64 & bits64 & bits64 & value
      , bits64 & value & bits64 & bits64 & bits64 & value
      , bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & bits64 & bits64 & bits64 & bits64 & value
      , bits64 & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , value & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & value
      , bits64 & float64 & bits64
      , bits64 & float64 & bits64 & bits64
      , (bits64 & float64 & value) & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & (bits64 & bits64) & bits64 & bits64 & value
      , (bits64 & float64)
        & (bits64 & bits64)
        & bits64
        & bits64
        & bits64
        & value
      , (bits64 & float64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & bits64 & bits64 & bits64 & bits64 & value )] Elt = sig
  type t : k mod external_

  val create_for_pool : unit -> t
end

(* not external *)
module type%template [@kind k = (value & value)] Elt = sig
  type t : k

  val create_for_pool : unit -> t
end

module type%template
  [@kind
    k
    = ( value
      , bits64
      , value & value
      , bits64 & bits64
      , bits64 & bits64 & bits64
      , value & value & bits64 & bits64 & bits64 & value
      , value & value & value & value & value & value
      , (bits64 & float64)
        & (bits64 & bits64)
        & bits64
        & bits64
        & bits64
        & value
      , value & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & (bits64 & bits64) & bits64 & bits64 & value
      , bits64 & value & bits64 & bits64 & bits64 & value
      , bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & bits64 & bits64 & bits64 & bits64 & value
      , bits64 & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , value & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & value
      , bits64 & float64 & bits64
      , bits64 & float64 & bits64 & bits64
      , (bits64 & float64 & value) & bits64 & bits64 & bits64 & value
      , (bits64 & float64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & bits64 & bits64 & bits64 & bits64 & value )] S = sig
  module Elt : Elt [@kind k]

  type t

  val create : ?chunk_size:local_ int -> unit -> t
  val alloc : t -> Ptr.t
  val free : t -> Ptr.t -> unit
  val get : t -> Ptr.t -> Elt.t
  val set : t -> Ptr.t -> Elt.t -> unit
  val iter : t -> f:(Ptr.t -> unit) @ local -> unit
  val outstanding : t @ local -> int
end

module type%template
  [@kind
    k
    = ( bits64
      , value & value
      , bits64 & bits64
      , bits64 & bits64 & bits64
      , (bits64 & float64)
        & (bits64 & bits64)
        & bits64
        & bits64
        & bits64
        & value
      , (bits64 & float64) & (bits64 & bits64) & bits64 & bits64 & value
      , value & value & bits64 & bits64 & bits64 & value
      , value & value & value & value & value & value
      , value & bits64 & bits64 & bits64 & bits64 & value
      , bits64 & value & bits64 & bits64 & bits64 & value
      , bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & bits64 & bits64 & bits64 & bits64 & value
      , bits64 & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , value & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & value
      , bits64 & float64 & bits64
      , bits64 & float64 & bits64 & bits64
      , (bits64 & float64 & value) & bits64 & bits64 & bits64 & value
      , (bits64 & float64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & bits64 & bits64 & bits64 & bits64 & value )] S_global = sig
  module Elt : Elt [@kind k]

  val alloc : unit -> Ptr.t
  val free : Ptr.t -> unit
  val get : Ptr.t -> Elt.t
  val set : Ptr.t -> Elt.t -> unit
  val iter : f:(Ptr.t -> unit) @ local -> unit
end

module type Pool = sig
  module type%template
    [@kind
      k
      = ( bits64
        , value & value
        , bits64 & bits64
        , bits64 & bits64 & bits64
        , value & value & bits64 & bits64 & bits64 & value
        , value & value & value & value & value & value
        , value & bits64 & bits64 & bits64 & bits64 & value
        , bits64 & value & bits64 & bits64 & bits64 & value
        , bits64 & bits64 & bits64 & bits64 & bits64 & value
        , (bits64 & bits64) & value & bits64 & bits64 & bits64 & value
        , (bits64 & bits64)
          & (bits64 & bits64)
          & bits64
          & bits64
          & bits64
          & value
        , (bits64 & bits64) & bits64 & bits64 & bits64 & bits64 & value
        , (bits64 & float64) & (bits64 & bits64) & bits64 & bits64 & value
        , bits64 & (bits64 & bits64) & bits64 & bits64 & bits64 & value
        , (bits64 & float64)
          & (bits64 & bits64)
          & bits64
          & bits64
          & bits64
          & value
        , value & (bits64 & bits64) & bits64 & bits64 & bits64 & value
        , bits64 & float64 & value
        , bits64 & float64 & bits64
        , bits64 & float64 & bits64 & bits64
        , (bits64 & float64 & value) & bits64 & bits64 & bits64 & value
        , (bits64 & float64 & bits64) & bits64 & bits64 & bits64 & value
        , bits64 & float64 & bits64 & bits64 & bits64 & bits64 & bits64 & value
        , (bits64 & float64) & value & bits64 & bits64 & bits64 & value
        , (bits64 & float64) & bits64 & bits64 & bits64 & bits64 & value )] S =
    S [@kind k]

  module%template
    [@kind
      k
      = ( value
        , bits64
        , value & value
        , bits64 & bits64
        , bits64 & bits64 & bits64
        , value & value & bits64 & bits64 & bits64 & value
        , value & value & value & value & value & value
        , value & bits64 & bits64 & bits64 & bits64 & value
        , bits64 & value & bits64 & bits64 & bits64 & value
        , bits64 & bits64 & bits64 & bits64 & bits64 & value
        , (bits64 & bits64) & value & bits64 & bits64 & bits64 & value
        , (bits64 & bits64)
          & (bits64 & bits64)
          & bits64
          & bits64
          & bits64
          & value
        , (bits64 & bits64) & bits64 & bits64 & bits64 & bits64 & value
        , (bits64 & float64) & (bits64 & bits64) & bits64 & bits64 & value
        , bits64 & (bits64 & bits64) & bits64 & bits64 & bits64 & value
        , (bits64 & float64)
          & (bits64 & bits64)
          & bits64
          & bits64
          & bits64
          & value
        , value & (bits64 & bits64) & bits64 & bits64 & bits64 & value
        , bits64 & float64 & value
        , bits64 & float64 & bits64
        , bits64 & float64 & bits64 & bits64
        , (bits64 & float64 & value) & bits64 & bits64 & bits64 & value
        , (bits64 & float64 & bits64) & bits64 & bits64 & bits64 & value
        , bits64 & float64 & bits64 & bits64 & bits64 & bits64 & bits64 & value
        , (bits64 & float64) & value & bits64 & bits64 & bits64 & value
        , (bits64 & float64) & bits64 & bits64 & bits64 & bits64 & value )] Make
      (Arg : Elt
    [@kind k]) : S [@kind k] with module Elt = Arg

  module%template
    [@kind
      k
      = ( bits64
        , value & value
        , bits64 & bits64
        , bits64 & bits64 & bits64
        , value & value & bits64 & bits64 & bits64 & value
        , value & value & value & value & value & value
        , value & bits64 & bits64 & bits64 & bits64 & value
        , bits64 & value & bits64 & bits64 & bits64 & value
        , bits64 & bits64 & bits64 & bits64 & bits64 & value
        , (bits64 & bits64) & value & bits64 & bits64 & bits64 & value
        , (bits64 & bits64)
          & (bits64 & bits64)
          & bits64
          & bits64
          & bits64
          & value
        , (bits64 & bits64) & bits64 & bits64 & bits64 & bits64 & value
        , bits64 & (bits64 & bits64) & bits64 & bits64 & bits64 & value
        , value & (bits64 & bits64) & bits64 & bits64 & bits64 & value
        , (bits64 & float64) & (bits64 & bits64) & bits64 & bits64 & value
        , bits64 & float64 & value
        , bits64 & float64 & bits64
        , (bits64 & float64)
          & (bits64 & bits64)
          & bits64
          & bits64
          & bits64
          & value
        , bits64 & float64 & bits64 & bits64
        , (bits64 & float64 & value) & bits64 & bits64 & bits64 & value
        , (bits64 & float64 & bits64) & bits64 & bits64 & bits64 & value
        , bits64 & float64 & bits64 & bits64 & bits64 & bits64 & bits64 & value
        , (bits64 & float64) & value & bits64 & bits64 & bits64 & value
        , (bits64 & float64) & bits64 & bits64 & bits64 & bits64 & value )] Make_global
      (Arg : Elt
    [@kind k]) : S_global [@kind k] with module Elt = Arg
end
