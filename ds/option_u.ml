open! Core
open! Unboxed
open Option_u_intf

module type%template
  [@kind
    k
    = ( value
      , value & value
      , bits64
      , bits64 & bits64
      , value & bits64
      , bits64 & value
      , value & bits64 & bits64
      , (bits64 & bits64) & value
      , (bits64 & bits64) & bits64 & bits64
      , (bits64 & float64) & value
      , (bits64 & float64) & bits64
      , (bits64 & float64) & bits64 & bits64
      , value & bits64 & float64
      , bits64 & bits64 & float64 )] S = S [@kind k]

module%template
  [@kind
    k
    = ( value
      , value & value
      , bits64
      , bits64 & bits64
      , value & bits64
      , bits64 & value
      , value & bits64 & bits64
      , (bits64 & bits64) & value
      , (bits64 & bits64) & bits64 & bits64
      , (bits64 & float64) & value
      , (bits64 & float64) & bits64
      , (bits64 & float64) & bits64 & bits64
      , value & bits64 & float64
      , bits64 & bits64 & float64 )] Make
    (Arg : Elt
  [@kind k]) : S [@kind k] with module Elt = Arg = struct
  module Elt = Arg

  type (_ : k) tag =
    | None : _ tag
    | Some : Elt.t tag

  type t : (immediate & k) mod internal = T : #('a tag * 'a) -> t [@@unboxed]

  let[@inline] none () : t = T #(None, Elt.trivial_create_for_none ())
  let[@inline] some (v : Elt.t) : t = T #(Some, v)

  let[@inline] is_none = function
    | T #(None, _) -> true
    | T #(Some, _) -> false
  ;;

  let[@inline] is_some t = not (is_none t)

  let[@inline] value (t : t) ~(default : (unit -> Elt.t) @ local) : Elt.t =
    match t with
    | T #(None, _) -> default ()
    | T #(Some, v) -> v
  ;;

  let[@inline] value_exn (t : t) : Elt.t =
    match t with
    | T #(None, _) ->
      let _ = failwith "value_exn on none" in
      Elt.trivial_create_for_none ()
    | T #(Some, v) -> v
  ;;

  module Optional_syntax = struct
    module Optional_syntax = struct
      let[@inline] is_none t = is_none t
      let[@inline] unsafe_value t = value_exn t
    end
  end
end
