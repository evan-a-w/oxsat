open! Core
open! Import

module Input = struct
  module Op = Formula.Op

  module Term = struct
    include Formula

    type t = any [@@deriving sexp_of]
    type op = Op.t
  end

  module Class_metadata = struct
    type t = unit [@@deriving compare, sexp]

    module Query = struct
      type t = unit [@@deriving compare, sexp, hash]
    end

    module Binary_query = struct
      type t = unit [@@deriving compare, sexp, hash]
    end

    let matches () ~query:() = true
    let matches_binary () () ~query:() = true
    let empty = ()
    let add () ~op:_ ~args:_ = ()
    let remove () ~op:_ ~args:_ = ()
  end
end

module Graph = Egx.Graph.Make (Input)
module Pattern = Egx.Pattern.Make (Graph)
