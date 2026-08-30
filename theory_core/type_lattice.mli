open! Core
open! Feel.Import

val is_subtype : Type_expr.t -> of_:Type_expr.t -> bool
val meet : Type_expr.t -> Type_expr.t -> Type_expr.t option
val disjoint : Type_expr.t -> Type_expr.t -> bool
val strict_supertypes : Type_expr.t -> Type_expr.t list
