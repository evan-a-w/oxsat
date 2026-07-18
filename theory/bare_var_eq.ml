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
  ; candidates : Pair.Hash_set.t
  ; candidate_injected : Pair.Hash_set.t
  ; type_forward_injected : Pair.Hash_set.t
  ; la_forward_fst_injected : Pair.Hash_set.t
  ; la_forward_snd_injected : Pair.Hash_set.t
  ; la_reverse_injected : Pair.Hash_set.t
  }

let create () =
  { registered = Pair.Hash_set.create ()
  ; candidates = Pair.Hash_set.create ()
  ; candidate_injected = Pair.Hash_set.create ()
  ; type_forward_injected = Pair.Hash_set.create ()
  ; la_forward_fst_injected = Pair.Hash_set.create ()
  ; la_forward_snd_injected = Pair.Hash_set.create ()
  ; la_reverse_injected = Pair.Hash_set.create ()
  }
;;

let register t a b = Hash_set.add t.registered (normalize (a, b))

let register_candidate t a b =
  let pair = normalize (a, b) in
  Hash_set.add t.registered pair;
  Hash_set.add t.candidates pair
;;

let uf_atom a b : Atom.t = `Eq (Formula.Var a, Formula.Var b)
let type_atom a b : Atom.t = `Type_eq (Type_expr.Var a, Type_expr.Var b)

let is_numeric : Type_expr.t option -> bool = function
  | Some (Base (Int | Float)) -> true
  | _ -> false
;;

(* The two [`Le] atoms whose conjunction encodes numeric equality of the pair. *)
let la_le_atoms (a, b) : Atom.t * Atom.t =
  let diff = Linear_expr.(var a - var b) in
  `Le (diff, Q.zero), `Le (Linear_expr.neg diff, Q.zero)
;;

(* eq -> type_eq *)
let type_forward_clause (a, b) = [ type_atom a b, true; uf_atom a b, false ]

(* eq -> le, for one of the two [le]s encoding numeric equality *)
let la_forward_clause (a, b) le = [ le, true; uf_atom a b, false ]

(* (le1 /\ le2) -> eq: numeric coincidence of two numeric values forces
   equality, so a false eq propagates a disequality into LA *)
let la_reverse_clause (a, b) =
  let le1, le2 = la_le_atoms (a, b) in
  [ uf_atom a b, true; le1, false; le2, false ]
;;

let find_ready t ~injected ~is_ready =
  Hash_set.find t.registered ~f:(fun pair ->
    (not (Hash_set.mem injected pair)) && is_ready pair)
;;

(* Every emitted lemma must be "productive": currently violated, unit, or
   introducing a fresh atom. The host solver stops polling for lemmas after a
   clause that neither propagates nor conflicts, and if no unassigned variables
   remain it then declares Sat -- so returning an already-satisfied clause can
   permanently mask a violated one behind it. Each attempt below therefore
   checks the theory-side truth of its clause's atoms and skips (without
   marking, so it is reconsidered when values change) whenever the clause is
   currently satisfied. *)
let maybe_get_lemma t ~eq_value ~type_eq_value ~le_value ~theory_of ~get_type =
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
  let type_eq_value (a, b) = type_eq_value a b in
  let eq_is value pair = [%equal: bool option] (eq_value pair) (Some value) in
  let fire ~injected ~is_ready ~clause =
    match find_ready t ~injected ~is_ready with
    | None -> None
    | Some pair ->
      Hash_set.add injected pair;
      Some (clause pair)
  in
  (* A theory-discovered shared pair gets the reverse/split clause
     [eq \/ ~le1 \/ ~le2] while the eq atom has no value (typically it doesn't
     exist yet -- this clause is what creates it), so the SAT solver is forced
     to decide the pair's arrangement -- delayed theory combination. Once the eq
     atom is assigned, the value-gated attempts below take over. Sound even for
     pairs that merely coincided in a theory's model by chance; see
     [Branch_and_bound.equality_candidates]. *)
  let candidate_attempt () =
    let is_ready ((a, b) as pair) =
      Option.is_none (eq_value pair)
      && la_member a
      && la_member b
      &&
      let le1, le2 = la_le_atoms pair in
      (not ([%equal: bool option] (le_value le1) (Some false)))
      && not ([%equal: bool option] (le_value le2) (Some false))
    in
    match
      Hash_set.find t.candidates ~f:(fun pair ->
        (not (Hash_set.mem t.candidate_injected pair)) && is_ready pair)
    with
    | None -> None
    | Some pair ->
      Hash_set.add t.candidate_injected pair;
      Hash_set.add t.la_reverse_injected pair;
      Some (la_reverse_clause pair)
  in
  let attempts =
    [ candidate_attempt
    ; (fun () ->
        fire
          ~injected:t.type_forward_injected
          ~is_ready:(fun ((a, b) as pair) ->
            eq_is true pair
            && (type_member a || type_member b)
            && not ([%equal: bool option] (type_eq_value pair) (Some true)))
          ~clause:type_forward_clause)
    ; (fun () ->
        fire
          ~injected:t.la_forward_fst_injected
          ~is_ready:(fun ((a, b) as pair) ->
            eq_is true pair
            && (la_member a || la_member b)
            &&
            let le1, _ = la_le_atoms pair in
            not ([%equal: bool option] (le_value le1) (Some true)))
          ~clause:(fun pair ->
            let le1, _ = la_le_atoms pair in
            la_forward_clause pair le1))
    ; (fun () ->
        fire
          ~injected:t.la_forward_snd_injected
          ~is_ready:(fun ((a, b) as pair) ->
            eq_is true pair
            && (la_member a || la_member b)
            &&
            let _, le2 = la_le_atoms pair in
            not ([%equal: bool option] (le_value le2) (Some true)))
          ~clause:(fun pair ->
            let _, le2 = la_le_atoms pair in
            la_forward_clause pair le2))
    ; (fun () ->
        fire
          ~injected:t.la_reverse_injected
          ~is_ready:(fun ((a, b) as pair) ->
            eq_is false pair
            && la_member a
            && la_member b
            &&
            let le1, le2 = la_le_atoms pair in
            (not ([%equal: bool option] (le_value le1) (Some false)))
            && not ([%equal: bool option] (le_value le2) (Some false)))
          ~clause:la_reverse_clause)
    ]
  in
  match List.find_map attempts ~f:(fun attempt -> attempt ()) with
  | Some lemma -> `Lemma lemma
  | None -> `Consistent
;;
