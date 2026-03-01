open! Core

type t =
  { inc : float
  ; decay_factor : float
  ; rescale : float
  }

let unit t = t.inc
let decay t = { t with inc = t.inc /. t.decay_factor }
let rescale t = { t with inc = t.inc /. t.rescale }
let default () = { inc = 1.0; decay_factor = 0.95; rescale = 1e20 }
