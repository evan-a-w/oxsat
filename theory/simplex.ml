open! Core
open! Feel.Import

type t = { coefficients : Q.t Vec.Value.t Vec.Value.t }

let add_atom t x =
  Vec.Value.iter t.coefficients ~f:(fun v ->
    Vec.Value.fill_to_length v ~length:(x + 1) ~f:(fun _ -> Q.zero))
;;
