open! Core
open! Feel.Import

(** Fresh variables reserved for solver-generated terms. The generated string
    contains an internal prefix that ordinary client-created names should not
    use, plus a process-wide monotonic counter. *)
val create : ?hint:string -> unit -> Tvar.t
