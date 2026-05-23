open! Core
open! Import

type t : ((value & value) & value & value) mod external_ =
  #{ reason : Reason.t
   ; decision_level : int
   ; literal : int
   }
(* reason is actually [external_], idk why I need this *)
[@@unsafe_allow_any_mode_crossing]

module Option_u :
  Option_u.S [@kind (value & value) & value & value] with type Elt.t := t

module Vec : Vec.S [@kind (value & value) & value & value] with type Elt.t := t
