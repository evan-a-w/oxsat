open! Core
open! Feel.Import

(** Lazily connects a fresh arithmetic variable to the UF term it abstracts. *)
type t

val create : unit -> t
val register : t -> tvar:Tvar.t -> term:Formula.any -> unit
val mem : t -> Tvar.t -> bool
val maybe_get_lemma : t -> [ `Consistent | `Lemma of (Atom.t * bool) list ]
