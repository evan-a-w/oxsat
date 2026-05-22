open! Core
open! Import

type entry : ((value & value) & value) mod external_ =
  #{ reason : Reason.t
   ; decision_level : int
   }
(* reason is actually [external_], idk why I need this *)
[@@unsafe_allow_any_mode_crossing]

type (_ : (value & value) & value) tag =
  | No_entry : #(#(unit * unit) * unit) tag
  | Entry : entry tag

type t = T : #('a tag * 'a) -> t [@@unbosed]

let no_entry () = T #(No_entry, #(#((), ()), ()))
let entry entry = T #(Entry, entry)
