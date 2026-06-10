open! Core
open! Feel.Import
include Interned.Global_string

let to_string = unintern
let of_string = intern
