open! Core
open! Feel.Import

module Term = struct
  type t =
    [ `App of function_:Tvar.t * args:t list
    | `Var of Tvar.t
    ]
  [@@deriving sexp, compare, hash]

  include functor Comparable.Make
  include functor Hashable.Make
end

module Atom = struct
  type t = [ `Eq of Term.t * Term.t ] [@@deriving sexp, compare, hash]

  let normalize = function
    | `Eq (a, b) as x ->
      (match Ordering.of_int ([%compare: Term.t] a b) with
       | Equal | Less -> x
       | Greater -> `Eq (b, a))
  ;;

  include functor Comparable.Make
  include functor Hashable.Make
end

module Atom_data = struct
  type t =
    { atom : Atom.t
    ; sat_var : int
    ; mutable assignment : bool or_null [@compare.ignore]
    }
  [@@deriving hash, compare, sexp]
end

module Trail_entry = struct
  type t =
    #{ undo_entry : Ufdsu.Undo_entry.t
     ; decision_level : int
     }

  let create_for_vec () =
    #{ undo_entry = #{ child = 0; new_root = 0; rank_incremented = false }
     ; decision_level = 0
     }
  ;;

  include functor Vecable.Make [@kind (value & value & value) & value]
end

type t =
  { ufds_var_by_term : int Term.Table.t
  ; ufdsu : Ufdsu.t
  ; atoms : Atom_data.t Atom.Table.t
  ; trail : Trail_entry.Vec.t
  ; mutable trail_entry_processed_till : int
  }

let create ~atoms =
  { ufds_var_by_term = Term.Table.create ()
  ; ufdsu = Ufdsu.create ()
  ; atoms =
      List.map atoms ~f:(fun (atom, sat_var) : (Atom.t * Atom_data.t) ->
        let atom = Atom.normalize atom in
        atom, { atom; sat_var; assignment = Null })
      |> Atom.Table.of_alist_exn
  ; trail = Trail_entry.Vec.create ()
  ; trail_entry_processed_till = 0
  }
;;

(* let assert_atom t ~decision_level ~(atom : Atom.t) ~value = *)
(* let atom = Atom.normalize atom in *)
(* match atom, value with *)
(* | `Eq (term1, term2), true -> () *)
(* ;; *)
