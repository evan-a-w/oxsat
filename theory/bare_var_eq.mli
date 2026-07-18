open! Core
open! Feel.Import

(** Lazy cross-theory consequences of an ambiguous [Eq (Var a, Var b)]. *)
type t

val create : unit -> t
val register : t -> Tvar.t -> Tvar.t -> unit

(** Returns one not-yet-injected consequence of a currently assigned bridge
    equality, gated on the theory membership of its endpoints ([theory_of],
    where [Shared] counts as a member of every theory):

    - a true equality whose endpoint is in the type theory forces the type
      equality;
    - a true equality whose endpoint is numeric (in LA, or with a numeric type)
      forces numeric equality;
    - a false equality with both endpoints numeric forces numeric disequality
      (via "numeric coincidence implies equality").

    Returned lemmas are permanent, so each consequence is emitted at most once
    per pair. *)
val maybe_get_lemma
  :  t
  -> eq_value:(Tvar.t -> Tvar.t -> bool option)
  -> theory_of:(Tvar.t -> Formula.Theory.Packed.t option)
  -> get_type:(Tvar.t -> Type_expr.t option)
  -> [ `Consistent | `Lemma of (Atom.t * bool) list ]
