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
  }
[@@deriving sexp_of]

(** Verifies that every formula in [asserted_formulas] evaluates to true under
    the model's atom values, and that each assigned linear/type atom's truth
    value agrees with the numeric/type witnesses in [tvar_assignments]. Returns
    an error naming the first formula not forced true, or the first atom whose
    value contradicts its witness. EUF equalities are checked at the boolean
    level only. *)
val check : t -> asserted_formulas:Formula.any list -> unit Or_error.t
