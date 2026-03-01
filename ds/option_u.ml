open! Core
open Option_u_intf

module type S = Option_u_intf.S

module Make (Arg : Elt) = struct
  module Elt = Arg

  type t = Elt.t option

  let none () = None
  let some x = Some x
  let is_none = Option.is_none
  let is_some = Option.is_some
  let value t ~default = Option.value t ~default:(default ())

  let value_exn = function
    | Some x -> x
    | None -> failwith "Option_u.value_exn on none"
  ;;

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = value_exn
    end
  end
end
