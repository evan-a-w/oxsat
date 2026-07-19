open! Core
open! Import

(** A checkable model of a satisfiable query. [atom_values] records the boolean
    value the solver assigned to each theory atom (unassigned atoms are absent);
    [tvar_assignments] records what each theory determined about each variable.
    Together these let {!check} independently confirm the model satisfies the
    asserted formulas. *)
type t =
  { atom_values : bool Atom.Map.t
  ; tvar_assignments : Tvar_assignment.t Tvar.Map.t
  ; euf_classes : Formula.any Formula.Any.Map.t
  }
[@@deriving sexp_of]

(** Verifies that every formula in [asserted_formulas] evaluates to true under
    the model's atom values, and that each assigned atom's truth value agrees
    with its witness: linear atoms against the numeric values, type equalities
    against the assigned types, and EUF equalities against [euf_classes] (whose
    equivalence relation is also checked to be a genuine congruence). Returns an
    error naming the first inconsistency. *)
val check : t -> asserted_formulas:Formula.any list -> unit Or_error.t
