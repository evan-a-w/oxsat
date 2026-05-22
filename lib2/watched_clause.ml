open! Core
open! Import

type t =
  #{ clause_idx : int
   ; blocker : int
   }
[@@deriving fields]

let create ~clause_idx ~blocker = #{ clause_idx; blocker }
let create_for_vec () = #{ clause_idx = 0; blocker = 0 }

include functor Vecable.Make [@kind value & value]
