open! Core

type t =
  #{ iterations : int
   ; decisions : int
   ; propagations : int
   ; conflicts : int
   ; learned_clauses : int
   ; learned_clause_literals : int
   ; max_decision_level : int
   }
[@@deriving sexp, fields]
