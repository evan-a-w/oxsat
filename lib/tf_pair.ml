open! Core

type 'a t =
  { t : 'a
  ; f : 'a
  }
[@@deriving sexp]

let get t b = if b then t.t else t.f
let set t b x = if b then { t with t = x } else { t with f = x }
let create f = { t = f true; f = f false }
let create_local f = create f

let iter t ~f =
  f t.t;
  f t.f
;;

let iteri t ~f =
  f ~key:true ~data:t.t;
  f ~key:false ~data:t.f
;;

let foldi t ~init ~f =
  let acc = f ~key:true ~data:t.t ~acc:init in
  f ~key:false ~data:t.f ~acc
;;

let fold t ~init ~f = foldi t ~init ~f:(fun ~key:_ ~data ~acc -> f acc data)
