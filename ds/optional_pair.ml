open! Core
open Optional_pair_intf

module type S = Optional_pair_intf.S

module Make (Fst_arg : Elt) (Snd_arg : Elt) = struct
  module Fst = Fst_arg
  module Snd = Snd_arg

  type t = (Fst.t * Snd.t) option

  let none () = None
  let some (fst, snd) = Some (fst, snd)
  let is_none = Option.is_none
  let is_some = Option.is_some
  let value t ~default = Option.value t ~default:(default ())

  let value_exn = function
    | Some pair -> pair
    | None -> failwith "Optional_pair.value_exn on none"
  ;;

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = value_exn
    end
  end
end
