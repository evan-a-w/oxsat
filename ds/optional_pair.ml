open! Core
open! Unboxed
open Optional_pair_intf

module type%template
  [@kind
    k = (value, bits64, bits64 & bits64, bits64 & float64)
    , v = (value, bits64, bits64 & bits64)] S = S [@kind k v]

module%template
  [@kind
    k = (value, bits64, bits64 & bits64, bits64 & float64)
    , v = (value, bits64, bits64 & bits64)] Make
    (Fst_arg : Elt
  [@kind k])
    (Snd_arg : Elt
  [@kind v]) :
  S [@kind k v] with module Fst = Fst_arg and module Snd = Snd_arg = struct
  module Fst = Fst_arg
  module Snd = Snd_arg

  type (_ : k, _ : v) tag =
    | None : _ tag
    | Some : (Fst.t, Snd.t) tag

  type t : immediate & k & v = T : #(('a, 'b) tag * 'a * 'b) -> t [@@unboxed]

  let[@inline] none () : t =
    T #(None, Fst.trivial_create_for_none (), Snd.trivial_create_for_none ())
  ;;

  let[@inline] some #(fst, snd) : t = T #(Some, fst, snd)

  let[@inline] is_none = function
    | T #(None, _, _) -> true
    | T #(Some, _, _) -> false
  ;;

  let[@inline] is_some t = not (is_none t)

  let[@inline] value (t : t) ~(local_ default) : #(Fst.t * Snd.t) =
    match t with
    | T #(None, _, _) -> default ()
    | T #(Some, a, b) -> #(a, b)
  ;;

  let[@inline] value_exn (t : t) : #(Fst.t * Snd.t) =
    match t with
    | T #(None, _, _) ->
      let _ = failwith "value_exn on none" in
      #(Fst.trivial_create_for_none (), Snd.trivial_create_for_none ())
    | T #(Some, a, b) -> #(a, b)
  ;;

  module Optional_syntax = struct
    module Optional_syntax = struct
      let[@inline] is_none t = is_none t
      let[@inline] unsafe_value t = value_exn t
    end
  end
end
