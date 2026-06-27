open! Core
open! Import

type (_ : value mod external_) tag : value mod external_ =
  | Decision : unit tag
  | Clause_idx : int tag

type t : (value & value) mod external_ = T : #('a tag * 'a) -> t
[@@unboxed]
[@@unsafe_allow_any_mode_crossing]
(* Safe as long as noone puts non external_ values in the slot used for
   [Clause_idx].

   This is enforced in the interface, because [t] can't be constructed outside
   of the given constructors.
*)
[@@unsafe_allow_any_mode_crossing]

val decision : unit -> t
val clause_idx : int -> t
