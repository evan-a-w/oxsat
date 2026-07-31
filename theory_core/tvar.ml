open! Core
open! Feel.Import
include Interned.Global_string

let generated_suffix = "\000oxsat.generated"

let to_string t =
  let raw = unintern t in
  String.chop_suffix raw ~suffix:generated_suffix |> Option.value ~default:raw
;;

let of_string = intern
let sexp_of_t t = [%sexp_of: string] (to_string t)
let t_of_sexp sexp = [%of_sexp: string] sexp |> intern
