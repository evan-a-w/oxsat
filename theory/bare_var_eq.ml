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
  ; type_forward_injected : Pair.Hash_set.t
  ; la_forward_injected : Pair.Hash_set.t
  ; la_reverse_injected : Pair.Hash_set.t
  ; pending : (Atom.t * bool) list Queue.t
  }

let create () =
  { registered = Pair.Hash_set.create ()
  ; type_forward_injected = Pair.Hash_set.create ()
  ; la_forward_injected = Pair.Hash_set.create ()
  ; la_reverse_injected = Pair.Hash_set.create ()
  ; pending = Queue.create ()
  }
;;

let register t a b = Hash_set.add t.registered (normalize (a, b))
let uf_atom a b : Atom.t = `Eq (Formula.Var a, Formula.Var b)
let type_atom a b : Atom.t = `Type_eq (Type_expr.Var a, Type_expr.Var b)

let is_numeric : Type_expr.t option -> bool = function
  | Some (Base (Int | Float)) -> true
  | _ -> false
;;

let la_atoms a b : Atom.t list =
  let diff = Linear_expr.(var a - var b) in
  [ `Le (diff, Q.zero); `Le (Linear_expr.neg diff, Q.zero) ]
;;

(* eq -> type_eq *)
let type_forward_clauses (a, b) =
  [ [ type_atom a b, true; uf_atom a b, false ] ]
;;

(* eq -> le, for each of the two [le]s encoding numeric equality *)
let la_forward_clauses (a, b) =
  List.map (la_atoms a b) ~f:(fun le -> [ le, true; uf_atom a b, false ])
;;

(* (le1 /\ le2) -> eq: numeric coincidence of two numeric values forces
   equality, so a false eq propagates a disequality into LA *)
let la_reverse_clauses (a, b) =
  [ (uf_atom a b, true) :: List.map (la_atoms a b) ~f:(fun le -> le, false) ]
;;

let find_ready t ~injected ~is_ready =
  Hash_set.find t.registered ~f:(fun pair ->
    (not (Hash_set.mem injected pair)) && is_ready pair)
;;

let maybe_get_lemma t ~eq_value ~theory_of ~get_type =
  match Queue.dequeue t.pending with
  | Some lemma -> `Lemma lemma
  | None ->
    let member v theory =
      match theory_of v with
      | None -> false
      | Some packed -> Formula.Theory.Packed.includes packed theory
    in
    let type_member v = member v Formula.Theory.(Packed.T Type) in
    let la_member v =
      member v Formula.Theory.(Packed.T La) || is_numeric (get_type v)
    in
    let eq_value (a, b) = eq_value a b in
    let fire ~injected ~is_ready ~clauses =
      match find_ready t ~injected ~is_ready with
      | None -> None
      | Some pair ->
        Hash_set.add injected pair;
        (match clauses pair with
         | [] -> None
         | first :: rest ->
           List.iter rest ~f:(fun lemma -> Queue.enqueue t.pending lemma);
           Some first)
    in
    let attempts =
      [ (fun () ->
          fire
            ~injected:t.type_forward_injected
            ~is_ready:(fun ((a, b) as pair) ->
              [%equal: bool option] (eq_value pair) (Some true)
              && (type_member a || type_member b))
            ~clauses:type_forward_clauses)
      ; (fun () ->
          fire
            ~injected:t.la_forward_injected
            ~is_ready:(fun ((a, b) as pair) ->
              [%equal: bool option] (eq_value pair) (Some true)
              && (la_member a || la_member b))
            ~clauses:la_forward_clauses)
      ; (fun () ->
          fire
            ~injected:t.la_reverse_injected
            ~is_ready:(fun ((a, b) as pair) ->
              [%equal: bool option] (eq_value pair) (Some false)
              && la_member a
              && la_member b)
            ~clauses:la_reverse_clauses)
      ]
    in
    (match List.find_map attempts ~f:(fun attempt -> attempt ()) with
     | Some lemma -> `Lemma lemma
     | None -> `Consistent)
;;
