open! Core

module%template
  [@kind
    k
    = ( value
      , bits64
      , value & value
      , bits64 & bits64
      , bits64 & bits64 & bits64
      , value & value & bits64 & bits64 & bits64 & value
      , value & value & value & value & value & value
      , value & bits64 & bits64 & bits64 & bits64 & value
      , bits64 & value & bits64 & bits64 & bits64 & value
      , bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64)
        & (bits64 & bits64)
        & bits64
        & bits64
        & bits64
        & value
      , (bits64 & bits64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & bits64 & bits64 & bits64 & bits64 & value
      , bits64 & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , value & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & value
      , bits64 & float64 & bits64
      , bits64 & float64 & bits64 & bits64
      , (bits64 & float64 & value) & bits64 & bits64 & bits64 & value
      , (bits64 & float64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & (bits64 & bits64) & bits64 & bits64 & value )] Make
    (Arg : Pool_intf.Elt
  [@kind k]) =
struct
  module Pool = Pool.Make [@kind k] (Arg)
end
