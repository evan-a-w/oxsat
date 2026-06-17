open! Core

module Type : sig
  type t =
    | Int
    | Float
  [@@deriving sexp, compare, equal]
end

type t

val create : unit -> t
val push : t -> unit
val pop : t -> unit
val assert_type : t -> Tvar.t -> Type.t -> unit
val get_type : t -> Tvar.t -> Type.t option
