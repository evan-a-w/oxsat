open! Core
open! Import

type t =
  #{ clause_idx : int
   ; blocking_literal : int
       (* another literal in the clause, checked early to avoid updating watched literals *)
   ; is_binary : bool
   (* if the clause is binary, checked to quickly propagate units in the case one of the variables is unset *)
   }
[@@deriving fields]

let create ~clause_idx ~blocking_literal ~is_binary =
  #{ clause_idx; blocking_literal; is_binary }
;;

let create_for_vec () =
  #{ clause_idx = 0; blocking_literal = 0; is_binary = false }
;;

include functor Vecable.Make [@kind value & value & value]
