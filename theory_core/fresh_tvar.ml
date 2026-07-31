open! Core
open! Feel.Import

let next_id = ref 0
let suffix = "\000oxsat.generated"

let create ?(hint = "t") () =
  let id = !next_id in
  incr next_id;
  Tvar.of_string (sprintf "%s.%d%s" hint id suffix)
;;
