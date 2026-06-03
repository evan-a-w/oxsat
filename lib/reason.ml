open! Core
open! Import

type (_ : value mod external_) tag : value mod external_ =
  | Decision : unit tag
  | Clause_idx : int tag
  | Theory_clause_idx : int tag

type t : (value & value) mod external_ = T : #('a tag * 'a) -> t
[@@unboxed] [@@unsafe_allow_any_mode_crossing]

let decision () = T #(Decision, ())
let clause_idx idx = T #(Clause_idx, idx)
let theory_clause_idx idx = T #(Theory_clause_idx, idx)
