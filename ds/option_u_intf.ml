open! Core
open! Unboxed

module type%template
  [@kind
    k
    = ( value
      , value & value
      , bits64
      , bits64 & bits64
      , value & bits64
      , bits64 & value
      , value & (bits64 & bits64)
      , (bits64 & bits64) & value
      , (bits64 & bits64) & (bits64 & bits64)
      , (bits64 & float64) & value
      , (bits64 & float64) & bits64
      , (bits64 & float64) & (bits64 & bits64)
      , value & (bits64 & float64)
      , bits64 & (bits64 & float64) )] Elt = sig
  type t : k

  val trivial_create_for_none : unit -> t
end

module type%template
  [@kind
    k
    = ( value
      , value & value
      , bits64
      , bits64 & bits64
      , value & bits64
      , bits64 & value
      , value & (bits64 & bits64)
      , (bits64 & bits64) & value
      , (bits64 & bits64) & (bits64 & bits64)
      , (bits64 & float64) & value
      , (bits64 & float64) & bits64
      , (bits64 & float64) & (bits64 & bits64)
      , value & (bits64 & float64)
      , bits64 & (bits64 & float64) )] S = sig
  module Elt : Elt [@kind k]

  type (_ : k) tag =
    | None : _ tag
    | Some : Elt.t tag

  type t : immediate & k = T : #('a tag * 'a) -> t [@@unboxed]

  val none : unit -> t
  val some : Elt.t -> t
  val is_none : t -> bool
  val is_some : t -> bool
  val value : t -> default:(unit -> Elt.t) @ local -> Elt.t
  val value_exn : t -> Elt.t

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool
      val unsafe_value : t -> Elt.t
    end
  end
end

module type Option_u = sig
  module type%template
    [@kind
      k
      = ( value
        , value & value
        , bits64
        , bits64 & bits64
        , value & bits64
        , bits64 & value
        , value & (bits64 & bits64)
        , (bits64 & bits64) & value
        , (bits64 & bits64) & (bits64 & bits64)
        , (bits64 & float64) & value
        , (bits64 & float64) & bits64
        , (bits64 & float64) & (bits64 & bits64)
        , value & (bits64 & float64)
        , bits64 & (bits64 & float64) )] S = S [@kind k]

  module%template
    [@kind
      k
      = ( value
        , value & value
        , bits64
        , bits64 & bits64
        , value & bits64
        , bits64 & value
        , value & (bits64 & bits64)
        , (bits64 & bits64) & value
        , (bits64 & bits64) & (bits64 & bits64)
        , (bits64 & float64) & value
        , (bits64 & float64) & bits64
        , (bits64 & float64) & (bits64 & bits64)
        , value & (bits64 & float64)
        , bits64 & (bits64 & float64) )] Make
      (Arg : Elt
    [@kind k]) : S [@kind k] with module Elt = Arg
end
