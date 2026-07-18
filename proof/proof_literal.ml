open! Core

type t =
  { atom : Proof_atom.t
  ; positive : bool
  }
[@@deriving sexp_of, compare]

module Serialized = struct
  type t =
    { atom : Proof_atom.t
    ; positive : bool
    }
  [@@deriving sexp]
end

let create ~atom ~positive = { atom = Proof_atom.normalize atom; positive }

let t_of_sexp sexp =
  let { Serialized.atom; positive } = Serialized.t_of_sexp sexp in
  create ~atom ~positive
;;

let neg t = { t with positive = not t.positive }
