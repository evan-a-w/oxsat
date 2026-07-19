open! Core
open! Import

module Clause = struct
  type t = int array
end

module Clause_add_proof = struct
  type 'theory_proof t =
    | User_supplied
    | Consequence_of_last_conflict
      (* can check that the last conflcit entails the learned clause *)
    | Theory_lemma of 'theory_proof
end

module Entry = struct
  type 'theory_proof t =
    | Conflict
    | Unit_prop of { clause_idx : int }
    | Add_clause of 'theory_proof Clause_add_proof.t
end

type 'theory_proof t =
  { trace : 'theory_proof Entry.t Vec.Value.t
  ; clauses : Clause.t Vec.Value.t
  }
