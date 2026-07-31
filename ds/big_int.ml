open! Core

(* Arbitrary-precision signed integers.

   Representation: a sign (1 or -1) and a little-endian array of limbs in base
   [2^15]. Limbs are in [0, 2^15) and fit in a machine [int] even on 32-bit
   platforms; products of two limbs fit in an [int], so all arithmetic below is
   overflow-free. The value is [sign * sum_i limbs.(i) * 2^(15*i)]. Values are
   kept canonical: zero is always the empty limb array with sign 1, so there is
   no negative zero and no leading zero limbs. *)

let bits_per_limb = 15
let base = 1 lsl bits_per_limb

type t =
  { sign : int
  ; limbs : int array
  }
[@@deriving equal]

let hash t =
  let state = Hash.create () in
  let state = Hash.fold_int state t.sign in
  let state = Array.fold t.limbs ~init:state ~f:Hash.fold_int in
  Hash.get_hash_value state
;;

let zero = { sign = 1; limbs = [||] }
let one = { sign = 1; limbs = [| 1 |] }
let minus_one = { sign = -1; limbs = [| 1 |] }
let is_zero t = Array.length t.limbs = 0

let normalize_limbs limbs =
  let len = Array.length limbs in
  let rec last_nonzero i =
    if i < 0 then -1 else if limbs.(i) <> 0 then i else last_nonzero (i - 1)
  in
  match last_nonzero (len - 1) with
  | -1 -> [||]
  | i when i = len - 1 -> limbs
  | i -> Array.sub limbs ~pos:0 ~len:(i + 1)
;;

let mk sign limbs = if Array.length limbs = 0 then zero else { sign; limbs }

let of_int n =
  if n = 0
  then zero
  else (
    let sign = if n < 0 then -1 else 1 in
    let base_i64 = Int64.of_int base in
    let mag = ref (Int64.abs (Int64.of_int n)) in
    let limbs = ref [] in
    while Int64.( <> ) !mag 0L do
      limbs := Int64.to_int_trunc (Int64.rem !mag base_i64) :: !limbs;
      mag := Int64.( / ) !mag base_i64
    done;
    { sign; limbs = Array.of_list (List.rev !limbs) })
;;

let to_int t =
  if is_zero t
  then Some 0
  else (
    let base_i64 = Int64.of_int base in
    let mag = ref 0L in
    for i = Array.length t.limbs - 1 downto 0 do
      mag := Int64.( + ) (Int64.( * ) !mag base_i64) (Int64.of_int t.limbs.(i))
    done;
    let max_mag =
      if t.sign > 0
      then Int64.of_int Int.max_value
      else Int64.neg (Int64.of_int Int.min_value)
    in
    if Int64.( <= ) !mag max_mag
    then Int64.to_int (if t.sign > 0 then !mag else Int64.neg !mag)
    else None)
;;

let add_limbs x y =
  let len = max (Array.length x) (Array.length y) in
  let out = Array.create ~len:(len + 1) 0 in
  let carry = ref 0 in
  for i = 0 to len - 1 do
    let xi = if i < Array.length x then x.(i) else 0 in
    let yi = if i < Array.length y then y.(i) else 0 in
    let s = xi + yi + !carry in
    out.(i) <- s mod base;
    carry := s / base
  done;
  if !carry > 0 then out.(len) <- !carry;
  normalize_limbs out
;;

let sub_limbs x y =
  (* Requires the magnitude of [x] to be at least that of [y]. *)
  let out = Array.copy x in
  let borrow = ref 0 in
  for i = 0 to Array.length x - 1 do
    let yi = if i < Array.length y then y.(i) else 0 in
    let d = out.(i) - yi - !borrow in
    if d < 0
    then (
      out.(i) <- d + base;
      borrow := 1)
    else (
      out.(i) <- d;
      borrow := 0)
  done;
  assert (!borrow = 0);
  normalize_limbs out
;;

let compare_mag a b =
  let la = Array.length a.limbs in
  let lb = Array.length b.limbs in
  if la <> lb
  then Int.compare la lb
  else (
    let rec go i =
      if i < 0
      then 0
      else (
        match Int.compare a.limbs.(i) b.limbs.(i) with
        | 0 -> go (i - 1)
        | c -> c)
    in
    go (la - 1))
;;

let neg t = if is_zero t then zero else { sign = -t.sign; limbs = t.limbs }
let abs t = if is_zero t then zero else { sign = 1; limbs = t.limbs }

let add a b =
  if a.sign = b.sign
  then mk a.sign (add_limbs a.limbs b.limbs)
  else (
    match compare_mag a b with
    | 0 -> zero
    | c when c > 0 -> mk a.sign (sub_limbs a.limbs b.limbs)
    | _ -> mk b.sign (sub_limbs b.limbs a.limbs))
;;

let ( - ) a b = add a (neg b)
let succ t = add t one
let pred t = t - one

let mul a b =
  if is_zero a || is_zero b
  then zero
  else (
    let out =
      Array.create ~len:(Array.length a.limbs + Array.length b.limbs) 0
    in
    for i = 0 to Int.( - ) (Array.length a.limbs) 1 do
      let carry = ref 0 in
      for j = 0 to Int.( - ) (Array.length b.limbs) 1 do
        let acc = out.(i + j) + (a.limbs.(i) * b.limbs.(j)) + !carry in
        out.(i + j) <- acc mod base;
        carry := acc / base
      done;
      let k = ref (i + Array.length b.limbs) in
      while !carry <> 0 do
        assert (!k < Array.length out);
        let acc = out.(!k) + !carry in
        out.(!k) <- acc mod base;
        carry := acc / base;
        incr k
      done
    done;
    mk (a.sign * b.sign) (normalize_limbs out))
;;

let ( + ) = add
let ( * ) = mul

let shl1 t =
  if is_zero t
  then zero
  else (
    let n = Array.length t.limbs in
    let out = Array.create ~len:Int.(n + 1) 0 in
    let carry = ref 0 in
    for i = 0 to Int.( - ) n 1 do
      let limb = t.limbs.(i) in
      out.(i) <- (limb lsl 1) land Int.(base - 1) lor !carry;
      carry := limb lsr Int.( - ) bits_per_limb 1
    done;
    out.(n) <- !carry;
    mk 1 (normalize_limbs out))
;;

let get_bit t i = (t.limbs.(i / bits_per_limb) lsr (i mod bits_per_limb)) land 1

let set_low_bit t =
  if is_zero t
  then one
  else (
    let limbs = Array.copy t.limbs in
    limbs.(0) <- limbs.(0) lor 1;
    { sign = 1; limbs })
;;

let of_bits_lsb_first bits =
  let limbs = ref [] in
  let acc = ref 0 in
  let n = ref 0 in
  List.iter bits ~f:(fun bit ->
    if bit then acc := !acc lor (1 lsl !n);
    incr n;
    if !n = bits_per_limb
    then (
      limbs := !acc :: !limbs;
      acc := 0;
      n := 0));
  if !n > 0 then limbs := !acc :: !limbs;
  mk 1 (normalize_limbs (Array.of_list (List.rev !limbs)))
;;

let mag_div_rem a b =
  (* [a], [b] >= 0 and [b] > 0. Binary long division, one bit at a time.
     Quotient bits are collected least-significant-first. *)
  let num_bits = Int.( * ) bits_per_limb (Array.length a.limbs) in
  let r = ref zero in
  let q_bits = ref [] in
  for bit = Int.( - ) num_bits 1 downto 0 do
    r := shl1 !r;
    if get_bit a bit = 1 then r := set_low_bit !r;
    if compare_mag !r b >= 0
    then (
      r := mk 1 (sub_limbs !r.limbs b.limbs);
      q_bits := true :: !q_bits)
    else q_bits := false :: !q_bits
  done;
  of_bits_lsb_first !q_bits, !r
;;

let div_rem a b =
  if is_zero b then failwith "Big_int.div_rem: division by zero";
  let mq, mr = mag_div_rem (abs a) (abs b) in
  let q =
    if is_zero mq
    then zero
    else if a.sign = b.sign
    then mq
    else mk (-1) mq.limbs
  in
  let r =
    if is_zero mr then zero else if a.sign > 0 then mr else mk (-1) mr.limbs
  in
  q, r
;;

let ( / ) a b = fst (div_rem a b)
let ( % ) a b = snd (div_rem a b)

let gcd a b =
  let rec go a b = if is_zero b then a else go b (snd (div_rem a b)) in
  go (abs a) (abs b)
;;

let compare a b =
  let c = Int.compare a.sign b.sign in
  if c <> 0
  then c
  else (
    let m = compare_mag a b in
    if a.sign > 0 then m else Int.neg m)
;;

let sign' t = if is_zero t then 0 else t.sign
let sign t = if is_zero t then `Zero else if t.sign > 0 then `Pos else `Neg
let is_even t = is_zero t || t.limbs.(0) land 1 = 0
let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b

let of_string s =
  let len = String.length s in
  if len = 0 then failwith "Big_int.of_string: empty string";
  let sign, start =
    match s.[0] with
    | '-' -> -1, 1
    | '+' -> 1, 1
    | _ -> 1, 0
  in
  if start >= len then failwith "Big_int.of_string: missing digits";
  let acc = ref zero in
  for i = start to Int.( - ) len 1 do
    let c = s.[i] in
    if not (Char.is_digit c)
    then failwith (sprintf "Big_int.of_string: invalid character %C" c);
    let digit = Char.get_digit_exn c in
    acc := add (mul !acc (of_int 10)) (of_int digit)
  done;
  if sign < 0 then neg !acc else !acc
;;

let to_string t =
  if is_zero t
  then "0"
  else (
    let ten4 = of_int 10000 in
    let chunks = ref [] in
    let cur = ref (abs t) in
    while not (is_zero !cur) do
      let q, r = div_rem !cur ten4 in
      chunks
      := (match to_int r with
          | Some n -> n
          | None -> assert false)
         :: !chunks;
      cur := q
    done;
    let buf = Buffer.create 16 in
    if t.sign < 0 then Buffer.add_char buf '-';
    List.iteri !chunks ~f:(fun i chunk ->
      if i = 0
      then Buffer.add_string buf (Int.to_string chunk)
      else Buffer.add_string buf (Printf.sprintf "%04d" chunk));
    Buffer.contents buf)
;;

let sexp_of_t t = Sexp.Atom (to_string t)

let t_of_sexp = function
  | Sexp.Atom s -> of_string s
  | sexp -> of_sexp_error "Big_int.t_of_sexp: expected an atom" sexp
;;
