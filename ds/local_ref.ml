open! Core

type 'a t = { mutable value : 'a }

let create value = { value }
let get t = t.value
let set t value = t.value <- value
let ( ! ) = get
let ( := ) = set

module O = struct
  let ( ! ) = get
  let ( := ) = set
end
