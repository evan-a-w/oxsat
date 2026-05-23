open! Core
open! Import

type t : ((value & value) & value) mod external_ =
  #{ reason : Reason.t
   ; decision_level : int
   }
(* reason is actually [external_], idk why I need this *)
[@@unsafe_allow_any_mode_crossing]

let trivial_create_for_none () =
  #{ decision_level = 0; reason = T #(Decision, ()) }
;;

include functor Option_u.Make' [@kind (value & value) & value]
