open! Core
open! Feel.Import

(** Lazy cross-theory consequences of an ambiguous [Eq (Var a, Var b)]. *)
type t

val create : unit -> t
val register : t -> Tvar.t -> Tvar.t -> unit

(** Returns one not-yet-injected consequence for a currently true UF equality.
    Type equality is introduced once either endpoint is relevant to the type
    theory; linear equality is introduced once either endpoint has a numeric
    type. Returned lemmas are permanent, so each consequence is emitted at most
    once per pair. *)
val maybe_get_lemma
  :  t
  -> uf_equal:(Tvar.t -> Tvar.t -> bool option)
  -> type_is_relevant:(Tvar.t -> bool)
  -> get_type:(Tvar.t -> Type_expr.t option)
  -> [ `Consistent | `Lemma of (Atom.t * bool) list ]
