open! Core
open! Import

type t =
  #{ assignment : bool or_null
   ; watched_clauses : Watched_clause.Vec.t Tf_pair.t
   ; trail_entry : Trail_entry.Option_u.t
   }

let create_for_vec () =
  #{ watched_clauses =
       Tf_pair.create (fun (_ : bool) -> Watched_clause.Vec.create ())
   ; trail_entry = Trail_entry.Option_u.none ()
   ; assignment = Null
   }
;;

module Vec =
Vec.Make [@kind value & (value & value) & value & (value & value) & value] (struct
    type nonrec t = t

    let create_for_vec = create_for_vec
  end)
