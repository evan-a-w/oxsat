open! Core

type ('a : immediate) t = { mutable value : 'a }

let create value = exclave_ { value }

module O = struct
  let ( ! ) (local_ { value }) = value
  let ( := ) t x = t.value <- x
end

include O

let get = ( ! )
let set = ( := )
