open! Core
open! Import

type t =
  { mutable stamp : int
  ; seen_by_var : int Vec.Value.t
  }
[@@deriving sexp, fields]

let create () = { stamp = 0; seen_by_var = Vec.Value.create () }

let reset t =
  if t.stamp = Int.max_value
  then (
    Vec.Value.map_inplace t.seen_by_var ~f:(fun (_ : int) -> 0);
    t.stamp <- 1)
  else t.stamp <- t.stamp + 1
;;

let ensure_capacity t ~var =
  Vec.Value.fill_to_length t.seen_by_var ~length:(var + 1) ~f:(fun (_ : int) ->
    0)
;;

let is_seen t ~var =
  var < Vec.Value.length t.seen_by_var
  && Vec.Value.get t.seen_by_var var = t.stamp
;;

let mark_seen t ~var =
  ensure_capacity t ~var;
  Vec.Value.set t.seen_by_var var t.stamp
;;

let clear_seen t ~var =
  ensure_capacity t ~var;
  Vec.Value.set t.seen_by_var var 0
;;
