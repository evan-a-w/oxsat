open! Core

type t = private int [@@deriving sexp, equal, hash, compare]

val of_string : string -> t
val to_string : t -> string

include Hashable.S with type t := t
include Comparable.S with type t := t
