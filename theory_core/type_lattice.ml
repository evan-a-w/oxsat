open! Core
open! Feel.Import

let base_rank = function
  | Type_expr.Base.Int64 -> Some 0
  | Int -> Some 1
  | Real -> Some 2
  | Bool -> None
;;

let is_numeric_base_subtype sub ~of_ =
  match base_rank sub, base_rank of_ with
  | Some sub, Some super -> sub <= super
  | Some _, None | None, Some _ | None, None -> Type_expr.Base.equal sub of_
;;

let is_subtype sub ~of_ =
  match sub, of_ with
  | Type_expr.Base sub, Type_expr.Base super ->
    is_numeric_base_subtype sub ~of_:super
  | _ -> [%compare.equal: Type_expr.t] sub of_
;;

let meet a b =
  if is_subtype a ~of_:b
  then Some a
  else if is_subtype b ~of_:a
  then Some b
  else None
;;

let rec ground = function
  | Type_expr.Var _ | Type_of _ -> false
  | Base _ | Type -> true
  | App (_, args) -> List.for_all args ~f:ground
  | Function_type (arg, result) | Array_type (arg, result) ->
    ground arg && ground result
;;

let ground_mismatch a b =
  ground a && ground b && not ([%compare.equal: Type_expr.t] a b)
;;

let disjoint a b =
  match a, b with
  | Type_expr.Var _, _
  | _, Type_expr.Var _
  | Type_expr.Type_of _, _
  | _, Type_expr.Type_of _ -> false
  | Type_expr.Base _, Type_expr.Base _ -> Option.is_none (meet a b)
  | Type_expr.App (left, left_args), Type_expr.App (right, right_args) ->
    (not (Tvar.equal left right))
    || List.length left_args <> List.length right_args
    || List.exists2_exn left_args right_args ~f:ground_mismatch
  | ( Type_expr.Function_type (left_arg, left_result)
    , Function_type (right_arg, right_result) )
  | ( Type_expr.Array_type (left_arg, left_result)
    , Array_type (right_arg, right_result) ) ->
    ground_mismatch left_arg right_arg
    || ground_mismatch left_result right_result
  | Type_expr.Type, Type_expr.Type -> false
  | ( Type_expr.Base _
    , ( Type_expr.App _
      | Type_expr.Function_type _
      | Type_expr.Array_type _
      | Type_expr.Type ) )
  | ( Type_expr.App _
    , ( Type_expr.Base _
      | Type_expr.Function_type _
      | Type_expr.Array_type _
      | Type_expr.Type ) )
  | ( Type_expr.Function_type _
    , ( Type_expr.Base _
      | Type_expr.App _
      | Type_expr.Array_type _
      | Type_expr.Type ) )
  | ( Type_expr.Array_type _
    , ( Type_expr.Base _
      | Type_expr.App _
      | Type_expr.Function_type _
      | Type_expr.Type ) )
  | ( Type_expr.Type
    , ( Type_expr.Base _
      | Type_expr.App _
      | Type_expr.Function_type _
      | Type_expr.Array_type _ ) ) -> true
;;

let strict_base_supertypes = function
  | Type_expr.Base.Int64 -> [ Type_expr.Base.Int; Real ]
  | Int -> [ Real ]
  | Real | Bool -> []
;;

let strict_supertypes = function
  | Type_expr.Base base ->
    List.map (strict_base_supertypes base) ~f:(fun base -> Type_expr.Base base)
  | Type_expr.Var _
  | Type_expr.Type_of _
  | Type_expr.App _
  | Type_expr.Function_type _
  | Type_expr.Array_type _
  | Type_expr.Type -> []
;;
