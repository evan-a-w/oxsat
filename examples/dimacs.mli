(** Parse a DIMACS-style string (with a leading header line) into a list of
    clauses, throwing away any zeros or unparsable tokens. *)
val read_string : string -> int list list

(** Read an entire file and parse it as above. *)
val read_file : string -> int list list

(** Given a list of clauses (int list list), emit a DIMACS "p cnf …" string. *)
val of_int_array_array : int list list -> string

(** DIMACS example constants - TODO: Replace stubs with actual data *)

val sudoku : string
val fail_eg : string
val succ_eg : string
val factor_1234321 : string
val factor_1235321 : string
val subsets_100 : string
