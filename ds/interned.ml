open! Core
include Interned_intf

let make (type a) (module Arg : Arg with type t = a) =
  (module struct
    type t = int [@@deriving sexp, hash, equal, compare]

    include functor Hashable.Make
    include functor Comparable.Make
    module Arg = Arg

    let tbl = Arg.Table.create ()
    let vec = Vec.Value.create ()

    let intern (arg : Arg.t) =
      Hashtbl.find_or_add tbl arg ~default:(fun () ->
        Vec.Value.push vec arg;
        Vec.Value.length vec - 1)
    ;;

    let unintern id = Vec.Value.get vec id

    let unsafe_clear_all () =
      Vec.Value.clear vec;
      Hashtbl.clear tbl
    ;;
  end : S
    with type Arg.t = a)
;;

module Global_string = (val make (module String) : S with type Arg.t = string)
