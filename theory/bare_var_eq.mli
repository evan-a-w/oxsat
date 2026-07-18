open! Core
open! Import

(** Lazy cross-theory consequences of an ambiguous [Eq (Var a, Var b)]. *)
type t

val create : unit -> t
val register : t -> Tvar.t -> Tvar.t -> unit

(** Registers a pair of Nelson-Oppen shared tvars that some theory discovered as
    possibly equal (e.g. coincident in the current LA model, or merged in the
    egraph). Unlike {!register}ed pairs, whose bridge clauses wait for the eq
    atom to be assigned, a candidate pair gets its reverse/split clause
    [eq \/ ~le1 \/ ~le2] injected directly -- creating the atom if needed and
    forcing the SAT solver to decide the pair's arrangement (delayed theory
    combination); the remaining bridge clauses follow via the usual value-gated
    injection once the eq atom is assigned. *)
val register_candidate : t -> Tvar.t -> Tvar.t -> unit

(** Returns one not-yet-injected bridge lemma for a registered pair, gated on
    the theory membership of its endpoints ([theory_of], where [Shared] counts
    as a member of every theory):

    - a true equality whose endpoint is in the type theory forces the type
      equality;
    - a true equality whose endpoint is numeric (in LA, or with a numeric type)
      forces numeric equality;
    - a false equality with both endpoints numeric forces numeric disequality
      (via "numeric coincidence implies equality");
    - a candidate pair's arrangement split (see {!register_candidate}).

    Returned lemmas are permanent, so each consequence is emitted at most once
    per pair. *)
val maybe_get_lemma
  :  t
  -> eq_value:(Tvar.t -> Tvar.t -> bool option)
  -> theory_of:(Tvar.t -> Formula.Theory.Packed.t option)
  -> get_type:(Tvar.t -> Type_expr.t option)
  -> [ `Consistent | `Lemma of (Atom.t * bool) list ]
