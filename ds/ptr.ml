open! Core

type t = int [@@deriving sexp]

let null () = -1
let is_null t = Int.equal t (null ())
let of_int x = x
let to_int x = x
let equal = Int.equal

module Option = struct
  type value = t
  type t = value option

  let none () = None
  let some v = Some v
  let unchecked_some v = Some v
  let some_is_representable _ = true
  let is_none = Option.is_none
  let is_some = Option.is_some
  let value t ~default = Option.value t ~default

  let unchecked_value = function
    | Some v -> v
    | None -> failwith "Ptr.Option.unchecked_value on none"
  ;;

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = unchecked_value
    end
  end
end

module Private = struct
  let chunk ~chunk_bits ptr = ptr lsr chunk_bits
  let idx ~chunk_bits ptr = ptr land ((1 lsl chunk_bits) - 1)
  let create ~chunk_bits ~chunk ~idx = (chunk lsl chunk_bits) lor idx
end
