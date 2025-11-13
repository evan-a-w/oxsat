open! Core
open! Import

module Literal_with_score = struct
  module T = struct
    type t =
      #{ literal : Literal.t
       ; score : F64.t
       }

    let create_for_rb () =
      #{ score = #0.; literal = Literal.create ~var:0 ~value:true }
    ;;

    let compare a b =
      match F64.compare a.#score b.#score with
      | 0 -> Literal.compare a.#literal b.#literal
      | o -> o
    ;;
  end

  include T

  module Rb =
    Rb.Make [@kind (bits64 & float64) value]
      (T)
      (struct
        type t = unit

        let create_for_rb () = ()
      end)
end

type t =
  { literals_with_score : Literal_with_score.Rb.t
  ; score_by_literal : F64.Option.Vec.t Tf_pair.t
  ; mutable adjusting_score : Adjusting_score.t
  }

let create () =
  { literals_with_score = Literal_with_score.Rb.create ()
  ; score_by_literal =
      Tf_pair.create (fun (_ : bool) -> F64.Option.Vec.create ())
  ; adjusting_score = Adjusting_score.default ()
  }
;;

let on_new_var t ~var =
  Tf_pair.iteri t.score_by_literal ~f:(fun ~key:value ~data:score_by_var ->
    F64.Option.Vec.fill_to_length
      score_by_var
      ~length:(var + 1)
      ~f:(fun (_ : int) -> F64.Option.none ());
    match%optional_u (F64.Option.Vec.get score_by_var var : F64.Option.t) with
    | Some _ -> ()
    | None ->
      let score = Adjusting_score.unit t.adjusting_score in
      F64.Option.Vec.set score_by_var var (F64.Option.some score);
      Literal_with_score.Rb.insert
        t.literals_with_score
        ~key:
          (#{ literal = Literal.create ~var ~value; score }
           : Literal_with_score.t)
        ~data:())
;;

let add_activity _t ~literal:(_ : Literal.t) = ()
let decay t = t.adjusting_score <- Adjusting_score.decay t.adjusting_score

let remove_from_pool t ~var =
  let rem value =
    Literal_with_score.Rb.remove
      t.literals_with_score
      #{ literal = Literal.create ~var ~value
       ; score =
           F64.Option.Vec.get (Tf_pair.get t.score_by_literal value) var
           |> F64.Option.value_exn
       }
  in
  rem true;
  rem false
;;

let add_to_pool t ~var =
  let add value =
    Literal_with_score.Rb.insert
      t.literals_with_score
      ~key:
        #{ literal = Literal.create ~var ~value
         ; score =
             F64.Option.Vec.get (Tf_pair.get t.score_by_literal value) var
             |> F64.Option.value_exn
         }
      ~data:()
  in
  add true;
  add false
;;

let choose_literal t =
  match%optional_u
    (Literal_with_score.Rb.max t.literals_with_score
     : Literal_with_score.Rb.Kv_option.t)
  with
  | None -> Literal.Option.none ()
  | Some kv ->
    let #(k, _) = kv in
    Literal.Option.some k.#literal
;;
