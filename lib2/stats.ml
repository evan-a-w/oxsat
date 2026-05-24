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

let empty () =
  #{ iterations = 0
   ; decisions = 0
   ; propagations = 0
   ; conflicts = 0
   ; learned_clauses = 0
   ; learned_clause_literals = 0
   ; max_decision_level = 0
   }
;;
