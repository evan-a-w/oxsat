open! Core

type t =
  { score_true : float Int.Table.t
  ; score_false : float Int.Table.t
  ; pool : Int.Hash_set.t
  ; mutable inc : float
  ; decay_factor : float
  }

let create () =
  { score_true = Int.Table.create ()
  ; score_false = Int.Table.create ()
  ; pool = Int.Hash_set.create ()
  ; inc = 1.0
  ; decay_factor = 0.95
  }
;;

let on_new_var t ~var =
  if not (Hashtbl.mem t.score_true var) then Hashtbl.set t.score_true ~key:var ~data:0.0;
  if not (Hashtbl.mem t.score_false var) then Hashtbl.set t.score_false ~key:var ~data:0.0;
  Hash_set.add t.pool var
;;

let add_activity t ~literal =
  let var = Literal.var literal in
  on_new_var t ~var;
  let table = if Literal.value literal then t.score_true else t.score_false in
  let old = Hashtbl.find table var |> Option.value ~default:0.0 in
  Hashtbl.set table ~key:var ~data:(old +. t.inc)
;;

let decay t = t.inc <- t.inc /. t.decay_factor
let remove_from_pool t ~var = Hash_set.remove t.pool var
let add_to_pool t ~var = Hash_set.add t.pool var

let choose_literal t =
  let best = ref None in
  Hash_set.iter t.pool ~f:(fun var ->
    let st = Hashtbl.find t.score_true var |> Option.value ~default:0.0 in
    let sf = Hashtbl.find t.score_false var |> Option.value ~default:0.0 in
    let cand = if Float.( >= ) st sf then Literal.create ~var ~value:true, st else Literal.create ~var ~value:false, sf in
    match !best with
    | None -> best := Some cand
    | Some (_, score) -> if Float.(snd cand > score) then best := Some cand);
  match !best with
  | None -> Literal.Option.none ()
  | Some (lit, _) -> Literal.Option.some lit
;;
