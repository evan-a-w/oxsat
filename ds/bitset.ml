open! Core

type t =
  { mutable words : int64 array
  }
[@@deriving sexp]

let bits_per_word = 64

let round_words bits =
  let bits = Int.max bits bits_per_word in
  (bits + bits_per_word - 1) / bits_per_word
;;

let create ?(size = bits_per_word) () =
  { words = Array.create ~len:(round_words size) 0L }
;;

let copy t = { words = Array.copy t.words }
let capacity t = Array.length t.words * bits_per_word

let ensure_capacity t bit =
  if bit < 0
  then invalid_arg "Bitset index must be non-negative"
  else if bit < capacity t
  then ()
  else (
    let needed_words = round_words (bit + 1) in
    let cur = Array.length t.words in
    let rec grow n = if n >= needed_words then n else grow (n * 2) in
    let new_len = grow (Int.max 1 cur) in
    let new_words = Array.create ~len:new_len 0L in
    Array.blit ~src:t.words ~src_pos:0 ~dst:new_words ~dst_pos:0 ~len:cur;
    t.words <- new_words)
;;

let word_and_mask bit =
  let word = bit / bits_per_word in
  let offset = bit mod bits_per_word in
  word, Stdlib.Int64.shift_left 1L offset
;;

let set t bit =
  ensure_capacity t bit;
  let word, mask = word_and_mask bit in
  t.words.(word) <- Stdlib.Int64.logor t.words.(word) mask
;;

let clear t bit =
  if bit < 0
  then invalid_arg "Bitset index must be non-negative"
  else if bit < capacity t
  then (
    let word, mask = word_and_mask bit in
    t.words.(word) <-
      Stdlib.Int64.logand t.words.(word) (Stdlib.Int64.lognot mask))
;;

let get t bit =
  if bit < 0
  then invalid_arg "Bitset index must be non-negative"
  else if bit >= capacity t
  then false
  else (
    let word, mask = word_and_mask bit in
    Stdlib.Int64.compare (Stdlib.Int64.logand t.words.(word) mask) 0L <> 0)
;;

let toggle t bit = if get t bit then clear t bit else set t bit

let clear_all t = Array.fill t.words ~pos:0 ~len:(Array.length t.words) 0L

let set_all t ~up_to =
  if up_to <= 0
  then clear_all t
  else (
    ensure_capacity t (up_to - 1);
    let full_words = up_to / bits_per_word in
    let rem_bits = up_to mod bits_per_word in
    for i = 0 to full_words - 1 do
      t.words.(i) <- Stdlib.Int64.lognot 0L
    done;
    if rem_bits > 0
    then
      t.words.(full_words) <-
        Stdlib.Int64.pred (Stdlib.Int64.shift_left 1L rem_bits);
    for i = full_words + 1 to Array.length t.words - 1 do
      t.words.(i) <- 0L
    done)
;;

let popcount t =
  let popcount_word w =
    let rec loop n acc =
      if Stdlib.Int64.equal n 0L
      then acc
      else
        loop
          (Stdlib.Int64.logand n (Stdlib.Int64.sub n 1L))
          (acc + 1)
    in
    loop w 0
  in
  Array.fold t.words ~init:0 ~f:(fun acc w -> acc + popcount_word w)
;;

let find_first_set t ~start_pos =
  let start_pos = Int.max 0 start_pos in
  let rec go i =
    if i >= capacity t
    then None
    else if get t i
    then Some i
    else go (i + 1)
  in
  go start_pos
;;

let find_first_clear t ~start_pos =
  let start_pos = Int.max 0 start_pos in
  let rec go i =
    if i >= capacity t
    then Some i
    else if get t i
    then go (i + 1)
    else Some i
  in
  go start_pos
;;

let ensure_capacity_to_match dest a b =
  let cap = Int.max (capacity a) (capacity b) in
  if cap > 0 then ensure_capacity dest (cap - 1)
;;

let bitwise_inplace ~dest a b ~op =
  ensure_capacity_to_match dest a b;
  let words_len = Array.length dest.words in
  for i = 0 to words_len - 1 do
    let wa = if i < Array.length a.words then a.words.(i) else 0L in
    let wb = if i < Array.length b.words then b.words.(i) else 0L in
    dest.words.(i) <- op wa wb
  done
;;

let land_inplace ~dest a b = bitwise_inplace ~dest a b ~op:Stdlib.Int64.logand
let lor_inplace ~dest a b = bitwise_inplace ~dest a b ~op:Stdlib.Int64.logor
let lxor_inplace ~dest a b = bitwise_inplace ~dest a b ~op:Stdlib.Int64.logxor

let mk_bitwise a b f =
  let dest = create ~size:(Int.max (capacity a) (capacity b)) () in
  f ~dest a b;
  dest
;;

let land_ a b = mk_bitwise a b land_inplace
let lor_ a b = mk_bitwise a b lor_inplace
let lxor_ a b = mk_bitwise a b lxor_inplace

let diff_inplace ~dest a b =
  bitwise_inplace ~dest a b ~op:(fun wa wb ->
    Stdlib.Int64.logand wa (Stdlib.Int64.lognot wb))
;;

let diff a b = mk_bitwise a b diff_inplace

let lnot_inplace t =
  for i = 0 to Array.length t.words - 1 do
    t.words.(i) <- Stdlib.Int64.lognot t.words.(i)
  done
;;

let lnot_ t =
  let out = copy t in
  lnot_inplace out;
  out
;;

let is_empty t = Array.for_all t.words ~f:(Stdlib.Int64.equal 0L)

let iter_set_bits t ~f =
  for i = 0 to capacity t - 1 do
    if get t i then f i
  done
;;

let to_set_bits_array t =
  let out = Vec.Value.create () in
  iter_set_bits t ~f:(Vec.Value.push out);
  Vec.Value.to_array out
;;

let of_set_bits_array bits =
  let max_bit = Array.fold bits ~init:(-1) ~f:Int.max in
  let t = create ~size:(Int.max bits_per_word (max_bit + 1)) () in
  Array.iter bits ~f:(set t);
  t
;;

let fold_set_bits t ~init ~f =
  let acc = ref init in
  iter_set_bits t ~f:(fun bit -> acc := f !acc bit);
  !acc
;;

let fold_set_bits_or_null t ~init ~f =
  let done_ = ref false in
  let acc = ref init in
  iter_set_bits t ~f:(fun bit -> if not !done_ then acc := f ~done_ !acc bit);
  !acc
;;
