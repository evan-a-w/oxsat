open! Core
open! Feel.Import

(** Encoding a bare-variable equality [Eq (Var a, Var b)] is ambiguous: [a] and
    [b] might turn out to be UF terms, or numbers, or both. The UF and type
    consequences of the equality are cheap and asserted eagerly regardless
    (see {!Encoding}), but the linear-arithmetic (La) consequence is only
    worth paying for once [a] and [b] are actually known to share a numeric
    ([Int] or [Float]) type -- otherwise every bare-variable equality in a
    formula would flood the simplex theory with constraints it never needs
    (this used to make purely-UF workloads dramatically slower).

    This module tracks bare-variable equalities registered via {!register}
    and, once both [uf_equal] reports the pair as equal and [get_type] reports
    a shared numeric type, returns a theory lemma forcing the La equality --
    an on-demand (lazy) theory combination step, rather than baking the La
    atoms into the initial encoding. *)

type t

val create : unit -> t

(** Registers [a] and [b] as an ambiguous bare-variable pair whose La equality
    may need to be injected later. Idempotent. *)
val register : t -> Tvar.t -> Tvar.t -> unit

(** Returns a lemma forcing the La equality for the first registered pair that
    is UF-equal and shares a numeric type but hasn't been handled yet, marking
    it handled so it isn't returned again. [uf_equal a b] should report the
    current truth value (if any) of [Eq (Var a, Var b)]; [get_type] the
    variable's current type. *)
val maybe_get_lemma
  :  t
  -> uf_equal:(Tvar.t -> Tvar.t -> bool option)
  -> get_type:(Tvar.t -> Type_expr.t option)
  -> [ `Consistent | `Lemma of (Atom.t * bool) list ]
