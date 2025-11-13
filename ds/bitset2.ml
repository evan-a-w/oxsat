open! Core
open! Unboxed

type fixed = Fixed
type variable = Variable

[%%template
type _ inner =
  | Fixed : #(I64.t array * int) -> fixed inner
  | Variable :
      { global_ arr : I64.t array ref
      ; global_ num_words : int ref
      }
      -> variable inner

type t = T : 'a inner -> t [@@unboxed]

let bits_per_word = 64

let[@mode m = (global, local)] create ?(size = 64) () =
  let num_words = (size + bits_per_word - 1) / bits_per_word in
  let num_words = if num_words = 0 then 1 else num_words in
  T
    (Variable
       { arr = ref (Array.create ~len:num_words #0L)
       ; num_words = ref num_words
       })
;;

let[@mode m = global] create_fixed ?(size = 64) () =
  let num_words = (size + bits_per_word - 1) / bits_per_word in
  let num_words = if num_words = 0 then 1 else num_words in
  T (Fixed #(Array.create ~len:num_words #0L, num_words))
;;

let[@mode m = local] create_fixed ?(size = 64) () = exclave_
  let num_words = (size + bits_per_word - 1) / bits_per_word in
  let num_words = if num_words = 0 then 1 else num_words in
  T (Fixed #(Array.create_local ~len:num_words #0L, num_words))
;;

let num_words = function
  | T (Fixed #(_, num_words)) -> num_words
  | T (Variable { num_words; _ }) -> !num_words
;;

let ensure_capacity t bit_pos =
  let required_word = bit_pos / bits_per_word in
  if required_word >= num_words t
  then (
    match t with
    | T (Fixed _) -> failwith "Can't resize fixed bitset"
    | T (Variable { num_words; arr }) ->
      let new_num_words = required_word + 1 in
      let new_arr = Array.create ~len:new_num_words #0L in
      for i = 0 to !num_words - 1 do
        new_arr.(i) <- !arr.(i)
      done;
      arr := new_arr;
      num_words := new_num_words)
;;]
