open! Core

(* @ocamlformat-disable *)
module%template
  [@kind
    k
    = ( value
      , value & value
      , value & (value & value) & value & (value & value) & value
      , (value & value) & value & value
      , bits64
      , float64
      , immediate & value & value
      , bits64 & bits64
      , value & value & value
      , bits64 & bits64 & bits64
      , bits64 & bits64 & immediate & immediate & bits64
      , bits64 & bits64 & value & value & bits64
      , (value & value & bits64) & bits64 & bits64 )] Make
    (Arg : Vec_intf.Elt
  [@kind k]) =
(* @ocamlformat-enable *)
struct
  module Vec = Vec.Make [@kind k] (Arg)
end
