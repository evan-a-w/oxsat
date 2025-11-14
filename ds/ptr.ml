open! Core
open! Unboxed

type t = I64.t [@@deriving sexp]

let null () = I64.max_value ()
let is_null t = I64.equal t (null ())
let of_int = I64.of_int
let to_int = I64.to_int_trunc
let equal = I64.equal

module Private = struct
  let chunk ~chunk_bits ptr =
    let t : I64.t = Obj.magic ptr in
    I64.(t lsr chunk_bits |> to_int_trunc)
  ;;

  let idx ~chunk_bits ptr =
    let t : I64.t = Obj.magic ptr in
    I64.(t land ((#1L lsl chunk_bits) - #1L) |> to_int_trunc)
  ;;

  let create ~chunk_bits ~chunk ~idx =
    let idx = I64.of_int idx in
    let chunk = I64.of_int chunk in
    I64.((chunk lsl chunk_bits) lor idx)
  ;;
end

module Option = I64.Option
