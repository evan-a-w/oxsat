open! Core
open! Feel.Import

module Term = struct
  type t =
    [ `App of function_:Tvar.t * arg:Tvar.t
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

type t =
  { ufds_var_by_term : int Term.Table.t
  ; ufdsu : Ufdsu.t
  ; atoms : Atom_data.t Atom.Table.t
  }

let create ~atoms =
  { ufds_var_by_term = Term.Table.create ()
  ; ufdsu = Ufdsu.create ()
  ; atoms =
      List.map atoms ~f:(fun (atom, sat_var) : (Atom.t * Atom_data.t) ->
        let atom = Atom.normalize atom in
        atom, { atom; sat_var; assignment = Null })
      |> Atom.Table.of_alist_exn
  }
;;

(* let assert_atom t ~decision_level ~(atom : Atom.t) ~value = *)
(* let atom = Atom.normalize atom in *)
(* match atom, value with *)
(* | `Eq (term1, term2), true -> () *)
(* ;; *)
