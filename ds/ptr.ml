open! Core
open! Unboxed

type t = I64.t

let null () = I64.max_value ()
let is_null t = I64.equal t (null ())
let of_int = I64.of_int
let to_int = I64.to_int_trunc
let equal = I64.equal

module Private = struct
  let chunk ptr =
    let t : I64.t = Obj.magic ptr in
    I64.(t lsr 48 |> to_int_trunc)
  ;;

  let idx ptr =
    let t : I64.t = Obj.magic ptr in
    I64.(t land ((#1L lsl 48) - #1L) |> to_int_trunc)
  ;;

  let create ~chunk ~idx =
    let idx = I64.of_int idx in
    let chunk = I64.of_int chunk in
    I64.((chunk lsl 48) lor idx)
  ;;
end

module Option = I64.Option

let sexp_of_t t =
  let chunk, idx = Private.chunk t, Private.idx t in
  [%sexp_of: chunk:int * idx:int] (~chunk, ~idx)
;;

let t_of_sexp sexp =
  let ~chunk, ~idx = [%of_sexp: chunk:int * idx:int] sexp in
  Private.create ~chunk ~idx
;;
