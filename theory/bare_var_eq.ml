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
  ; type_injected : Pair.Hash_set.t
  ; la_injected : Pair.Hash_set.t
  }

let create () =
  { registered = Pair.Hash_set.create ()
  ; type_injected = Pair.Hash_set.create ()
  ; la_injected = Pair.Hash_set.create ()
  }
;;

let register t a b = Hash_set.add t.registered (normalize (a, b))
let uf_atom a b : Atom.t = `Eq (Formula.Var a, Formula.Var b)
let type_atom a b : Atom.t = `Type_eq (Type_expr.Var a, Type_expr.Var b)
let type_lemma (a, b) = [ type_atom a b, true; uf_atom a b, false ]

let is_numeric : Type_expr.t option -> bool = function
  | Some (Base (Int | Float)) -> true
  | _ -> false
;;

let la_atoms a b : Atom.t list =
  let diff = Linear_expr.(var a - var b) in
  [ `Le (diff, Q.zero); `Le (Linear_expr.neg diff, Q.zero) ]
;;

let la_lemma (a, b) =
  List.map (la_atoms a b) ~f:(fun atom -> atom, true)
  @ [ uf_atom a b, false; type_atom a b, false ]
;;

let find_ready t ~injected ~is_ready =
  Hash_set.find t.registered ~f:(fun pair ->
    (not (Hash_set.mem injected pair)) && is_ready pair)
;;

let maybe_get_lemma
  t
  ~uf_equal
  ~type_is_relevant
  ~shared_la_is_relevant
  ~get_type
  =
  let uf_equal (a, b) = Option.value (uf_equal a b) ~default:false in
  let type_ready ((a, b) as pair) =
    uf_equal pair && (type_is_relevant a || type_is_relevant b)
  in
  match find_ready t ~injected:t.type_injected ~is_ready:type_ready with
  | Some pair ->
    Hash_set.add t.type_injected pair;
    `Lemma (type_lemma pair)
  | None ->
    let la_ready ((a, b) as pair) =
      uf_equal pair
      && (shared_la_is_relevant a
          || shared_la_is_relevant b
          || is_numeric (get_type a)
          || is_numeric (get_type b))
    in
    (match find_ready t ~injected:t.la_injected ~is_ready:la_ready with
     | None -> `Consistent
     | Some pair ->
       Hash_set.add t.la_injected pair;
       `Lemma (la_lemma pair))
;;
