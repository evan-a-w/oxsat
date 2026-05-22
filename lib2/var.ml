open! Core
open! Import

type t =
  #{ assignment : bool or_null
   ; watched_clauses : Watched_clause.Vec.t Tf_pair.t
   ; trail_entry : Trail_entry.t
   }

let create_for_vec () =
  #{ watched_clauses =
       Tf_pair.create (fun (_ : bool) -> Watched_clause.Vec.create ())
   ; trail_entry = Trail_entry.no_entry ()
   ; assignment = Null
   }
;;

include
  functor
  Vecable.Make
  [@kind value & value & value & (value & value) & value]
