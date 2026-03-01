open! Core

type t = int [@@deriving sexp]

let equal = Int.equal
let create ~var ~value = if value then Int.abs var else -Int.abs var
let var t = Int.abs t
let value t = t > 0
let compare = Int.compare
let to_int t = t
let of_int t = t
let negate t = -t

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
    | None -> failwith "Literal.Option.unchecked_value on none"
  ;;

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = unchecked_value
    end
  end
end
