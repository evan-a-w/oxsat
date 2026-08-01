open! Core
open! Import

let fresh_tvar ~hint () = Theory_core.Fresh_tvar.create ~hint ()

let ground_exn q ~context =
  match Formula.to_any q with
  | Some ground -> ground
  | None -> failwith context
;;

let ground_triggers_exn triggers =
  List.map triggers ~f:(fun trigger ->
    List.map trigger ~f:(ground_exn ~context:"nested quantifier in trigger"))
;;

let substitute_quantified subst formula =
  Or_error.ok_exn (Formula.substitute_quantified subst formula)
;;

let alpha_rename_quantified
  ~(bound : Tvar.t list)
  ~(triggers : Formula.quantified list list)
  ~(body : Formula.quantified)
  : Tvar.t list * Formula.any list list * Formula.quantified
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
    List.map triggers ~f:(fun trigger ->
      List.map trigger ~f:(substitute_quantified subst))
    |> ground_triggers_exn
  in
  let body = substitute_quantified subst body in
  bound, triggers, body
;;

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

let merge_trigger_groups a b =
  match a, b with
  | [], [] -> []
  | [], groups | groups, [] -> groups
  | _ ->
    List.concat_map a ~f:(fun group_a ->
      List.map b ~f:(fun group_b -> group_a @ group_b))
;;

let skolem_term ~(universals : Tvar.t list) skolem : Formula.any =
  match universals with
  | [] -> Var skolem
  | _ -> App (skolem, List.map universals ~f:(fun v -> Formula.Var v))
;;

let skolem_subst ~(universals : Tvar.t list) ~(bound : Tvar.t list)
  : Formula.any Tvar.Map.t
  =
  List.map bound ~f:(fun v ->
    v, skolem_term ~universals (fresh_tvar ~hint:"%skolem" ()))
  |> Tvar.Map.of_alist_exn
;;

module Prenexed_body = struct
  type t =
    { bound : Tvar.t list
    ; triggers : Formula.any list list
    ; body : Formula.any
    }
end

let register_guarded_forall
  (axioms : Quantifier_axiom.Axiom.t list ref)
  ~(bound : Tvar.t list)
  ~(triggers : Formula.any list list)
  ~(body : Formula.any)
  : Formula.any
  =
  let guard : Formula.any =
    Eq (Var (fresh_tvar ~hint:"%guard" ()), Var (fresh_tvar ~hint:"%guard" ()))
  in
  let guard = { Quantifier_axiom.Guard.atom = guard; polarity = Positive } in
  axioms
  := { Quantifier_axiom.Axiom.guard = Some guard; bound; triggers; body }
     :: !axioms;
  guard.atom
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
  | Not f -> go axioms ~polarity:(not polarity) (Formula.widen_quantified f)
  | And fs ->
    let fs =
      List.map fs ~f:(fun f -> go axioms ~polarity (Formula.widen_quantified f))
    in
    if polarity then And fs else Or fs
  | Or fs ->
    let fs =
      List.map fs ~f:(fun f -> go axioms ~polarity (Formula.widen_quantified f))
    in
    if polarity then Or fs else And fs
  | Forall (bound, triggers, body) ->
    if polarity
    then
      register_forall_axiom
        axioms
        ~bound
        ~triggers:(List.map triggers ~f:(List.map ~f:Formula.widen_quantified))
        ~body:(Formula.widen_quantified body)
        ~polarity:true
    else (
      let subst = skolem_subst ~universals:[] ~bound in
      substitute_quantified subst (Formula.widen_quantified body)
      |> go axioms ~polarity:false)
  | Exists (bound, body) ->
    if polarity
    then (
      let subst = skolem_subst ~universals:[] ~bound in
      substitute_quantified subst (Formula.widen_quantified body)
      |> go axioms ~polarity:true)
    else
      register_forall_axiom
        axioms
        ~bound
        ~triggers:[]
        ~body:(Formula.widen_quantified body)
        ~polarity:false
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
    let ground =
      ground_exn
        (Formula.widen_quantified atom)
        ~context:"nested quantifier in non-boolean position"
    in
    if polarity then ground else Not ground

and register_forall_axiom
  axioms
  ~(bound : Tvar.t list)
  ~(triggers : Formula.quantified list list)
  ~(body : Formula.quantified)
  ~(polarity : bool)
  : Formula.any
  =
  let bound, triggers, body = alpha_rename_quantified ~bound ~triggers ~body in
  let inner = go_axiom_body axioms ~polarity ~universals:bound body in
  register_guarded_forall
    axioms
    ~bound:(bound @ inner.Prenexed_body.bound)
    ~triggers:(merge_trigger_groups triggers inner.Prenexed_body.triggers)
    ~body:inner.Prenexed_body.body

and go_axiom_body
  (axioms : Quantifier_axiom.Axiom.t list ref)
  ~(polarity : bool)
  ~(universals : Tvar.t list)
  (formula : Formula.quantified)
  : Prenexed_body.t
  =
  let no_bound body = { Prenexed_body.bound = []; triggers = []; body } in
  let combine bodies ~op =
    { Prenexed_body.bound =
        List.concat_map bodies ~f:(fun b -> b.Prenexed_body.bound)
    ; triggers =
        List.fold bodies ~init:[] ~f:(fun triggers body ->
          merge_trigger_groups triggers body.Prenexed_body.triggers)
    ; body = op (List.map bodies ~f:(fun b -> b.Prenexed_body.body))
    }
  in
  match formula with
  | True -> no_bound (if polarity then True else False)
  | False -> no_bound (if polarity then False else True)
  | Not f ->
    go_axiom_body
      axioms
      ~polarity:(not polarity)
      ~universals
      (Formula.widen_quantified f)
  | And fs ->
    let bodies =
      List.map fs ~f:(fun f ->
        go_axiom_body axioms ~polarity ~universals (Formula.widen_quantified f))
    in
    combine bodies ~op:(fun fs -> if polarity then And fs else Or fs)
  | Or fs ->
    let bodies =
      List.map fs ~f:(fun f ->
        go_axiom_body axioms ~polarity ~universals (Formula.widen_quantified f))
    in
    combine bodies ~op:(fun fs -> if polarity then Or fs else And fs)
  | Forall (bound, triggers, body) ->
    if polarity
    then (
      let bound, triggers, body =
        alpha_rename_quantified
          ~bound
          ~triggers:
            (List.map triggers ~f:(List.map ~f:Formula.widen_quantified))
          ~body:(Formula.widen_quantified body)
      in
      let inner =
        go_axiom_body
          axioms
          ~polarity:true
          ~universals:(universals @ bound)
          body
      in
      { Prenexed_body.bound = bound @ inner.Prenexed_body.bound
      ; triggers = merge_trigger_groups triggers inner.Prenexed_body.triggers
      ; body = inner.Prenexed_body.body
      })
    else (
      let subst = skolem_subst ~universals ~bound in
      substitute_quantified subst (Formula.widen_quantified body)
      |> go_axiom_body axioms ~polarity:false ~universals)
  | Exists (bound, body) ->
    if polarity
    then (
      let subst = skolem_subst ~universals ~bound in
      substitute_quantified subst (Formula.widen_quantified body)
      |> go_axiom_body axioms ~polarity:true ~universals)
    else (
      let bound, _triggers, body =
        alpha_rename_quantified
          ~bound
          ~triggers:[]
          ~body:(Formula.widen_quantified body)
      in
      let inner =
        go_axiom_body
          axioms
          ~polarity:false
          ~universals:(universals @ bound)
          body
      in
      { inner with Prenexed_body.bound = bound @ inner.bound })
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
    let ground =
      ground_exn
        (Formula.widen_quantified atom)
        ~context:"nested quantifier in non-boolean position"
    in
    no_bound (if polarity then ground else Not ground)
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

let register_toplevel_forall
  ~(bound : Tvar.t list)
  ~(triggers : Formula.any list list)
  ~(body : Formula.any)
  : Quantifier_axiom.Axiom.t * Formula.quantified
  =
  let bound, triggers, body = alpha_rename ~bound ~triggers ~body in
  let axiom = { Quantifier_axiom.Axiom.guard = None; bound; triggers; body } in
  ( axiom
  , Forall
      ( bound
      , List.map triggers ~f:(List.map ~f:Formula.widen_quantified)
      , Formula.widen_quantified body ) )
;;

module Toplevel_prefix = struct
  type t =
    { given : Formula.quantified
    ; ground : Formula.any option
    ; axiom : Quantifier_axiom.Axiom.t option
    }
end

module Toplevel_prefix_body = struct
  type t =
    { given : Formula.quantified
    ; universals : Tvar.t list
    ; triggers : Formula.any list list
    ; stable_body : Formula.any
    }
end

let add_subst subst pairs =
  List.fold pairs ~init:subst ~f:(fun subst (key, data) ->
    Map.set subst ~key ~data)
;;

let rec register_toplevel_prefix_body
  ~(universals : Tvar.t list)
  ~(triggers : Formula.any list list)
  ~(subst : Formula.any Tvar.Map.t)
  (formula : Formula.quantified)
  : Toplevel_prefix_body.t Or_error.t
  =
  match formula with
  | Forall (bound, raw_triggers, body) ->
    let bound, raw_triggers, body =
      alpha_rename_quantified
        ~bound
        ~triggers:
          (List.map raw_triggers ~f:(List.map ~f:Formula.widen_quantified))
        ~body:(Formula.widen_quantified body)
    in
    let%bind.Or_error stable_triggers =
      Or_error.all
        (List.map raw_triggers ~f:(fun trigger ->
           Or_error.all
             (List.map trigger ~f:(fun term ->
                let%bind.Or_error term =
                  Formula.substitute_quantified
                    subst
                    (Formula.widen_quantified term)
                in
                match Formula.to_any term with
                | Some ground -> Ok ground
                | None -> Or_error.error_string "nested quantifier in trigger"))))
    in
    let%map.Or_error inner =
      register_toplevel_prefix_body
        ~universals:(universals @ bound)
        ~triggers:(merge_trigger_groups triggers stable_triggers)
        ~subst
        body
    in
    { inner with
      given =
        Forall
          ( bound
          , List.map raw_triggers ~f:(List.map ~f:Formula.widen_quantified)
          , inner.given )
    }
  | Exists (bound, body) ->
    let bound, _triggers, body =
      alpha_rename_quantified
        ~bound
        ~triggers:[]
        ~body:(Formula.widen_quantified body)
    in
    let skolem_pairs = Map.to_alist (skolem_subst ~universals ~bound) in
    let subst = add_subst subst skolem_pairs in
    let%map.Or_error inner =
      register_toplevel_prefix_body ~universals ~triggers ~subst body
    in
    { inner with given = Exists (bound, inner.given) }
  | _ ->
    (match Formula.to_any formula with
     | None ->
       Or_error.error_string
         "top-level proof route only supports a quantifier prefix with a \
          ground body"
     | Some body ->
       Ok
         { given = Formula.widen_quantified body
         ; universals
         ; triggers
         ; stable_body = Formula.substitute subst body
         })
;;

let register_toplevel_prefix formula =
  let%map.Or_error result =
    register_toplevel_prefix_body
      ~universals:[]
      ~triggers:[]
      ~subst:Tvar.Map.empty
      formula
  in
  match result.universals with
  | [] ->
    { Toplevel_prefix.given = result.given
    ; ground = Some result.stable_body
    ; axiom = None
    }
  | universals ->
    { given = result.given
    ; ground = None
    ; axiom =
        Some
          { Quantifier_axiom.Axiom.guard = None
          ; bound = universals
          ; triggers = result.triggers
          ; body = result.stable_body
          }
    }
;;

let elaborate (formula : Formula.quantified)
  : Formula.any * Quantifier_axiom.Axiom.t list
  =
  let axioms = ref [] in
  let ground = go axioms ~polarity:true formula in
  ground, List.rev !axioms
;;
