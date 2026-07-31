open! Core
open! Import

let fresh_tvar ~hint () = Theory_core.Fresh_tvar.create ~hint ()

(* Fresh capture-avoiding renaming of a universal's bound variables, so two
   axioms reusing the same source name (and any Skolem constants) never collide. *)
let alpha_rename
  ~(bound : Tvar.t list)
  ~(triggers : Formula.any list list)
  ~(body : Formula.any)
  : Tvar.t list * Formula.any list list * Formula.any
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
  bound, triggers, body
;;

let register_forall
  (axioms : Quantifier_axiom.Axiom.t list ref)
  ~(bound : Tvar.t list)
  ~(triggers : Formula.any list list)
  ~(body : Formula.any)
  : Formula.any
  =
  let bound, triggers, body = alpha_rename ~bound ~triggers ~body in
  let guard : Formula.any =
    Eq (Var (fresh_tvar ~hint:"%guard" ()), Var (fresh_tvar ~hint:"%guard" ()))
  in
  axioms
  := { Quantifier_axiom.Axiom.guard = Some guard; bound; triggers; body }
     :: !axioms;
  guard
;;

let register_toplevel_forall
  ~(bound : Tvar.t list)
  ~(triggers : Formula.any list list)
  ~(body : Formula.any)
  : Quantifier_axiom.Axiom.t * Formula.quantified
  =
  let bound, triggers, body = alpha_rename ~bound ~triggers ~body in
  let axiom = { Quantifier_axiom.Axiom.guard = None; bound; triggers; body } in
  axiom, Forall (bound, triggers, body)
;;

let skolemize_existential ~(bound : Tvar.t list) (body : Formula.any)
  : (Tvar.t * Formula.any) list * Formula.any
  =
  let pairs =
    List.map bound ~f:(fun v ->
      v, (Formula.Var (fresh_tvar ~hint:"%skolem" ()) : Formula.any))
  in
  let subst = Tvar.Map.of_alist_exn pairs in
  pairs, Formula.substitute subst body
;;

let skolemize ~(bound : Tvar.t list) (body : Formula.any) : Formula.any =
  snd (skolemize_existential ~bound body)
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
    | Ite _
    | App _
    | Select _
    | Store _
    | Datatype_constructor _
    | Datatype_selector _
    | Datatype_tester _
    | Bool
    | Int
    | Float
    | Type
    | Function_type _
    | Array_type _
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
