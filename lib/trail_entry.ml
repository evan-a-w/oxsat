open! Core
open! Import

type t : ((value & value) & value & value) mod external_ =
  #{ reason : Reason.t
   ; decision_level : int
   ; literal : int
   }
(* reason is actually [external_], idk why I need this *)
[@@unsafe_allow_any_mode_crossing]

let trivial_create_for_none () =
  #{ decision_level = 0; reason = T #(Decision, ()); literal = 0 }
;;

let create_for_vec = trivial_create_for_none

include functor Option_u.Make' [@kind (value & value) & value & value]
include functor Vecable.Make [@kind (value & value) & value & value]
