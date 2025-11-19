open! Core

type 'a t =
  #{ t : 'a
   ; f : 'a
   }

let%template get (t : _ @ m) (b : _ @ m) = if b then t.#t else t.#f
[@@mode m = (local, global)]
;;

let set t b x = if b then #{ t with t = x } else #{ t with f = x }
let create (f : _ @ local) = #{ t = f true; f = f false }

let create_local (local_ (f : _ -> _ @ local)) = exclave_
  #{ t = f true; f = f false }
;;

let iter t ~(f : _ @ local) =
  f t.#t;
  f t.#f
;;

let iteri t ~(f : _ @ local) =
  f ~key:true ~data:t.#t;
  f ~key:false ~data:t.#f
;;

let foldi t ~init ~(f : _ @ local) =
  let acc = f ~key:true ~data:t.#t ~acc:init in
  f ~key:false ~data:t.#f ~acc [@nontail]
;;

let fold t ~init ~(f : _ @ local) =
  foldi t ~init ~f:(fun ~key:_ ~data ~acc -> f acc data) [@nontail]
;;
