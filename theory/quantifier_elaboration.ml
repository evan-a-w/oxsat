open! Core
open! Import

(* Global, monotonically increasing so bound-variable renamings, Skolem
   constants, and guard atoms are unique across every call to [elaborate], not
   just within one. *)
let next_id = ref 0

let fresh_tvar ~hint () =
  let id = !next_id in
  incr next_id;
  Tvar.of_string (sprintf "%s.%d" hint id)
;;

let register_forall
  (axioms : Quantifier_axiom.Axiom.t list ref)
  ~(bound : Tvar.t list)
  ~(triggers : Formula.any list list)
  ~(body : Formula.any)
  : Formula.any
  =
  let renaming =
    List.map bound ~f:(fun v ->
      v, fresh_tvar ~hint:(Tvar.to_string v ^ ".bound") ())
  in
  let subst =
    List.map renaming ~f:(fun (old, new_) -> old, Formula.Var new_)
    |> Tvar.Map.of_alist_exn
  in
  let bound = List.map renaming ~f:snd in
  let triggers =
    List.map triggers ~f:(List.map ~f:(Formula.substitute subst))
  in
  let body = Formula.substitute subst body in
  let guard : Formula.any =
    Eq (Var (fresh_tvar ~hint:"%guard" ()), Var (fresh_tvar ~hint:"%guard" ()))
  in
  axioms := { Quantifier_axiom.Axiom.guard; bound; triggers; body } :: !axioms;
  guard
;;

let skolemize ~(bound : Tvar.t list) (body : Formula.any) : Formula.any =
  let subst =
    List.map bound ~f:(fun v -> v, Formula.Var (fresh_tvar ~hint:"%skolem" ()))
    |> Tvar.Map.of_alist_exn
  in
  Formula.substitute subst body
;;

let rec go
  (axioms : Quantifier_axiom.Axiom.t list ref)
  ~(polarity : bool)
  (formula : Formula.quantified)
  : Formula.any
  =
  match formula with
  | True -> if polarity then True else False
  | False -> if polarity then False else True
  | Not f -> go axioms ~polarity:(not polarity) f
  | And fs ->
    let fs = List.map fs ~f:(go axioms ~polarity) in
    if polarity then And fs else Or fs
  | Or fs ->
    let fs = List.map fs ~f:(go axioms ~polarity) in
    if polarity then Or fs else And fs
  | Forall (bound, triggers, body) ->
    if polarity
    then register_forall axioms ~bound ~triggers ~body
    else skolemize ~bound (Not body)
  | Exists (bound, body) ->
    if polarity
    then skolemize ~bound body
    else register_forall axioms ~bound ~triggers:[] ~body:(Not body)
  | ( Var _
    | Eq _
    | App _
    | Bool
    | Int
    | Float
    | Type
    | Function_type _
    | Type_of _
    | Type_var _
    | Type_app _
    | La_const _
    | La_scale_const _
    | La_add _
    | La_compare _ ) as atom ->
    (* Not recursed into: see the .mli's "nested inside a non-boolean-skeleton
       position" note. *)
    let ground = Formula.widen atom in
    if polarity then ground else Not ground
;;

let elaborate (formula : Formula.quantified)
  : Formula.any * Quantifier_axiom.Axiom.t list
  =
  let axioms = ref [] in
  let ground = go axioms ~polarity:true formula in
  ground, List.rev !axioms
;;
