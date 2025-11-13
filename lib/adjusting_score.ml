open! Core
open! Import
open! Unboxed

type t =
  #{ inc : F64.t
   ; decay_factor : F64.t
   ; rescale : F64.t
   }

let unit t = t.#inc
let decay t = #{ t with inc = F64.O.(t.#inc / t.#decay_factor) }
let default () = #{ inc = #1.0; decay_factor = #0.95; rescale = #1e20 }
