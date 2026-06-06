open! Core

module%template
  [@kind
    k
    = ( value
      , value & value
      , value & (value & value) & value & (value & value) & value
      , bits64
      , float64
      , (value & value) & value & value
      , (value & value & value) & value
      , immediate & value & value
      , bits64 & bits64
      , value & value & value
      , bits64 & bits64 & bits64
      , bits64 & bits64 & immediate & immediate & bits64
      , bits64 & bits64 & value & value & bits64
      , (value & value & bits64) & bits64 & bits64 )] Make
    (Arg : Vec_intf.Elt
  [@kind k]) =
struct
  module Vec = Vec.Make [@kind k] (Arg)
end
