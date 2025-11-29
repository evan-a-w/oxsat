open! Core
open! Unboxed

[%%template
type t =
  { mutable arr : I64.t array
  ; mutable num_words : int
  }

let bits_per_word = 64

let create ?(size = 0) () =
  let num_words = (size + bits_per_word - 1) / bits_per_word in
  let num_words = if num_words = 0 then 1 else num_words in
  { arr = Array.create ~len:num_words #0L; num_words }
;;

let[@inline] word_and_bit bit_pos =
  bit_pos / bits_per_word, bit_pos mod bits_per_word
;;

let ensure_capacity t bit_pos =
  let required_word = bit_pos / bits_per_word in
  if required_word >= t.num_words
  then (
    let new_num_words = required_word + 1 in
    let new_arr = Array.create ~len:new_num_words #0L in
    for i = 0 to t.num_words - 1 do
      new_arr.(i) <- t.arr.(i)
    done;
    t.arr <- new_arr;
    t.num_words <- new_num_words)
;;

let set t bit_pos =
  ensure_capacity t bit_pos;
  let word_idx, bit_offset = word_and_bit bit_pos in
  let mask = I64.(#1L lsl bit_offset) in
  t.arr.(word_idx) <- I64.(t.arr.(word_idx) lor mask)
;;

let clear t bit_pos =
  let word_idx, bit_offset = word_and_bit bit_pos in
  if word_idx < t.num_words
  then (
    let mask = I64.(lnot (#1L lsl bit_offset)) in
    t.arr.(word_idx) <- I64.(t.arr.(word_idx) land mask))
;;

let get t bit_pos =
  let word_idx, bit_offset = word_and_bit bit_pos in
  if word_idx >= t.num_words
  then false
  else (
    let mask = I64.(#1L lsl bit_offset) in
    not I64.(equal (t.arr.(word_idx) land mask) #0L))
;;

let toggle t bit_pos =
  ensure_capacity t bit_pos;
  let word_idx, bit_offset = word_and_bit bit_pos in
  let mask = I64.(#1L lsl bit_offset) in
  t.arr.(word_idx) <- I64.(t.arr.(word_idx) lxor mask)
;;

let clear_all t =
  for i = 0 to t.num_words - 1 do
    t.arr.(i) <- #0L
  done
;;

let set_all t ~up_to =
  ensure_capacity t up_to;
  let full_words = up_to / bits_per_word in
  for i = 0 to full_words - 1 do
    t.arr.(i) <- I64.(lnot #0L)
  done;
  let remaining_bits = up_to mod bits_per_word in
  if remaining_bits > 0
  then (
    let mask = I64.((#1L lsl remaining_bits) - #1L) in
    t.arr.(full_words) <- I64.(t.arr.(full_words) lor mask))
;;

let popcount t =
  let open Local_ref.O in
  let count = Local_ref.create 0 in
  for i = 0 to t.num_words - 1 do
    count := !count + (I64.popcount t.arr.(i) |> I64.to_int_trunc)
  done;
  !count [@nontail]
;;

let find_first_set t ~start_pos =
  let start_word = start_pos / bits_per_word in
  if start_word >= t.num_words
  then Null
  else (
    let result = ref Null in
    let word_idx = ref start_word in
    let start_bit_offset = start_pos mod bits_per_word in
    if start_bit_offset > 0
    then (
      let mask = I64.(lnot ((#1L lsl start_bit_offset) - #1L)) in
      let masked_word = I64.(t.arr.(!word_idx) land mask) in
      if not I64.(equal masked_word #0L)
      then
        result
        := This
             ((!word_idx * bits_per_word)
              + I64.(ctz masked_word |> to_int_trunc))
      else incr word_idx);
    while Or_null.is_null !result && !word_idx < t.num_words do
      let word = t.arr.(!word_idx) in
      if not I64.(equal word #0L)
      then
        result
        := This ((!word_idx * bits_per_word) + I64.(ctz word |> to_int_trunc));
      incr word_idx
    done;
    !result)
;;

let find_first_clear t ~start_pos =
  let start_word = start_pos / bits_per_word in
  if start_word >= t.num_words
  then This start_pos
  else (
    let result = ref Null in
    let word_idx = ref start_word in
    let start_bit_offset = start_pos mod bits_per_word in
    if start_bit_offset > 0
    then (
      let mask = I64.(lnot ((#1L lsl start_bit_offset) - #1L)) in
      let masked_word = I64.(t.arr.(!word_idx) lor lnot mask) in
      if not I64.(equal masked_word (lnot #0L))
      then
        result
        := This
             ((!word_idx * bits_per_word)
              + I64.(ctz (lnot masked_word) |> to_int_trunc))
      else incr word_idx);
    while Or_null.is_null !result && !word_idx < t.num_words do
      let word = t.arr.(!word_idx) in
      if not I64.(equal word (lnot #0L))
      then
        result
        := This
             ((!word_idx * bits_per_word)
              + I64.(ctz (lnot word) |> to_int_trunc));
      incr word_idx
    done;
    !result)
;;

let land_inplace ~dest t1 t2 =
  let min_words = min t1.num_words t2.num_words in
  if dest.num_words < min_words
  then ensure_capacity dest ((min_words * bits_per_word) - 1);
  for i = 0 to min_words - 1 do
    dest.arr.(i) <- I64.(t1.arr.(i) land t2.arr.(i))
  done;
  for i = min_words to dest.num_words - 1 do
    dest.arr.(i) <- #0L
  done
;;

let land_ t1 t2 =
  let result =
    create ~size:(min t1.num_words t2.num_words * bits_per_word) ()
  in
  land_inplace ~dest:result t1 t2;
  result
;;

let lor_inplace ~dest t1 t2 =
  let max_words = max t1.num_words t2.num_words in
  if dest.num_words < max_words
  then ensure_capacity dest ((max_words * bits_per_word) - 1);
  for i = 0 to max_words - 1 do
    let v1 = if i < t1.num_words then t1.arr.(i) else #0L in
    let v2 = if i < t2.num_words then t2.arr.(i) else #0L in
    dest.arr.(i) <- I64.(v1 lor v2)
  done
;;

let lor_ t1 t2 =
  let result =
    create ~size:(max t1.num_words t2.num_words * bits_per_word) ()
  in
  lor_inplace ~dest:result t1 t2;
  result
;;

let lxor_inplace ~dest t1 t2 =
  let max_words = max t1.num_words t2.num_words in
  if dest.num_words < max_words
  then ensure_capacity dest ((max_words * bits_per_word) - 1);
  for i = 0 to max_words - 1 do
    let v1 = if i < t1.num_words then t1.arr.(i) else #0L in
    let v2 = if i < t2.num_words then t2.arr.(i) else #0L in
    dest.arr.(i) <- I64.(v1 lxor v2)
  done
;;

let lxor_ t1 t2 =
  let result =
    create ~size:(max t1.num_words t2.num_words * bits_per_word) ()
  in
  lxor_inplace ~dest:result t1 t2;
  result
;;

let diff_inplace ~dest t1 t2 =
  let max_words = t1.num_words in
  if dest.num_words < max_words
  then ensure_capacity dest ((max_words * bits_per_word) - 1);
  for i = 0 to max_words - 1 do
    let v2 = if i < t2.num_words then t2.arr.(i) else #0L in
    dest.arr.(i) <- I64.(t1.arr.(i) land lnot v2)
  done;
  (* Clear any remaining words in dest if it's larger *)
  for i = max_words to dest.num_words - 1 do
    dest.arr.(i) <- #0L
  done
;;

let diff t1 t2 =
  let result = create ~size:(t1.num_words * bits_per_word) () in
  diff_inplace ~dest:result t1 t2;
  result
;;

let lnot_inplace t =
  for i = 0 to t.num_words - 1 do
    t.arr.(i) <- I64.(lnot t.arr.(i))
  done
;;

let copy t =
  let result = create ~size:(t.num_words * bits_per_word) () in
  ensure_capacity result t.num_words;
  for i = 0 to t.num_words - 1 do
    result.arr.(i) <- t.arr.(i)
  done;
  result
;;

let lnot_ t =
  let result = copy t in
  lnot_inplace result;
  result
;;

let is_empty t =
  let result = ref true in
  let i = ref 0 in
  while !result && !i < t.num_words do
    if not I64.(equal t.arr.(!i) #0L) then result := false;
    incr i
  done;
  !result
;;

let iter_set_bits t ~f =
  for word_idx = 0 to t.num_words - 1 do
    let rec loop word =
      if not I64.(equal word #0L)
      then (
        let bit_offset = I64.(ctz word |> to_int_trunc) in
        f ((word_idx * bits_per_word) + bit_offset);
        loop I64.(word land (word - #1L)))
    in
    loop t.arr.(word_idx)
  done
;;

let[@kind k = (value_or_null, bits64)]
   [@alloc a @ m = (stack_local, heap_global)] fold_set_bits
  (local_ t)
  ~(init : _ @ m)
  ~(local_ f)
  =
  (let rec fold_words word_idx acc =
     (if word_idx >= t.num_words
      then acc
      else (
        let[@exclave_if_stack a] rec fold_in_word word acc =
          (if I64.(equal word #0L)
           then acc
           else (
             let bit_offset = I64.(ctz word |> to_int_trunc) in
             let acc = f acc ((word_idx * bits_per_word) + bit_offset) in
             let word = I64.(word land (word - #1L)) in
             fold_in_word word acc))
          [@exclave_if_stack a]
        in
        let acc = fold_in_word t.arr.(word_idx) acc in
        fold_words (word_idx + 1) acc))
     [@exclave_if_stack a]
   in
   fold_words 0 init [@nontail])
  [@exclave_if_stack a]
;;

let fold_set_bits_or_null t ~init ~f = exclave_
  let open Local_ref.O in
  let done_ = Local_ref.create false in
  let rec fold_words word_idx acc = exclave_
    if !done_ || word_idx >= t.num_words
    then acc
    else (
      let rec fold_in_word word acc = exclave_
        if !done_ || I64.(equal word #0L)
        then acc
        else (
          let bit_offset = I64.(ctz word |> to_int_trunc) in
          let acc = f ~done_ acc ((word_idx * bits_per_word) + bit_offset) in
          let word = I64.(word land (word - #1L)) in
          fold_in_word word acc)
      in
      let acc = fold_in_word t.arr.(word_idx) acc in
      fold_words (word_idx + 1) acc)
  in
  fold_words 0 init
;;

let capacity t = t.num_words * bits_per_word

let to_set_bits_array t =
  let res = Vec.Value.create () in
  iter_set_bits t ~f:(fun i -> Vec.Value.push res i);
  Vec.Value.to_array res
;;

let of_set_bits_array arr =
  let res = create () in
  Array.iter arr ~f:(fun i -> set res i);
  res
;;

let sexp_of_t t = [%sexp_of: int array] (to_set_bits_array t)
let t_of_sexp sexp = of_set_bits_array ([%of_sexp: int array] sexp)]
