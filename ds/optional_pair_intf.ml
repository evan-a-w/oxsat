open! Core
open! Unboxed

module type%template
  [@kind k = (value, bits64, bits64 & bits64, bits64 & float64)] Elt = sig
  type t : k

  val trivial_create_for_none : unit -> t
end

module type%template
  [@kind
    k = (value, bits64, bits64 & bits64, bits64 & float64)
    , v = (value, bits64, bits64 & bits64)] S = sig
  module Fst : Elt [@kind k]
  module Snd : Elt [@kind v]

  type (_ : k, _ : v) tag =
    | None : _ tag
    | Some : (Fst.t, Snd.t) tag

  type t : immediate & k & v = T : #(('a, 'b) tag * 'a * 'b) -> t [@@unboxed]

  val none : unit -> t
  val some : #(Fst.t * Snd.t) -> t
  val is_none : t -> bool
  val is_some : t -> bool

  val value
    :  t
    -> default:(unit -> #(Fst.t * Snd.t)) @ local
    -> #(Fst.t * Snd.t)

  val value_exn : t -> #(Fst.t * Snd.t)

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool
      val unsafe_value : t -> #(Fst.t * Snd.t)
    end
  end
end

module type Optional_pair = sig
  module type%template
    [@kind
      k = (value, bits64, bits64 & bits64, bits64 & float64)
      , v = (value, bits64, bits64 & bits64)] S = S [@kind k v]

  module%template
    [@kind
      k = (value, bits64, bits64 & bits64, bits64 & float64)
      , v = (value, bits64, bits64 & bits64)] Make
      (A : Elt
    [@kind k])
      (B : Elt
    [@kind v]) : S [@kind k v] with module Fst = A and module Snd = B
end
