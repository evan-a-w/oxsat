open! Core
open! Feel

type t = unit

let create () = ()
let assert_literal () _ = ()
let check_consistent () = `Consistent
let pop () ~to_decision_level:_ = ()
let on_new_var () ~var:_ = ()
