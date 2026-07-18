open! Core
open! Feel.Import
include Interned.Global_string

let to_string = unintern
let of_string = intern
let sexp_of_t t = [%sexp_of: string] (unintern t)
let t_of_sexp sexp = [%of_sexp: string] sexp |> intern
