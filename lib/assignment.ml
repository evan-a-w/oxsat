open! Core
open! Import

type t =
  | Assigned_true
  | Assigned_false
  | Unassigned
[@@deriving sexp, equal]

module Raw_vec = Vec.Make [@kind value] (struct
    type nonrec t = t

    let create_for_vec () = Unassigned
  end)

module Vec = struct
  include Raw_vec

  type elt = t
end
