open! Core
open! Feel.Import

module Pair = struct
  type t = Tvar.t * Tvar.t [@@deriving compare, sexp, hash]

  include functor Comparable.Make
  include functor Hashable.Make
end

let normalize (a, b) = if Tvar.compare a b <= 0 then a, b else b, a

type t =
  { registered : Pair.Hash_set.t
  ; injected : Pair.Hash_set.t
  }

let create () = { registered = Pair.Hash_set.create (); injected = Pair.Hash_set.create () }
let register t a b = Hash_set.add t.registered (normalize (a, b))

let is_numeric : Type_expr.t option -> bool = function
  | Some (Base (Int | Float)) -> true
  | _ -> false
;;

(* Mirrors [Encoding.le_atoms_of_eq], specialized to two bare variables. *)
let le_atoms_of_var_eq a b : Atom.t list =
  let diff = Linear_expr.(var a - var b) in
  [ `Le (diff, Q.zero); `Le (Linear_expr.neg diff, Q.zero) ]
;;

let build_lemma (a, b) : (Atom.t * bool) list =
  List.map (le_atoms_of_var_eq a b) ~f:(fun atom -> atom, true)
  @ [ `Eq (Formula.Var a, Formula.Var b), false
    ; `Type_eq (Type_expr.Var a, Type_expr.Var b), false
    ]
;;

let maybe_get_lemma t ~uf_equal ~get_type =
  let ready =
    Hash_set.find t.registered ~f:(fun (a, b) ->
      (not (Hash_set.mem t.injected (a, b)))
      &&
      match uf_equal a b with
      | Some true -> is_numeric (get_type a)
      | Some false | None -> false)
  in
  match ready with
  | None -> `Consistent
  | Some pair ->
    Hash_set.add t.injected pair;
    `Lemma (build_lemma pair)
;;
