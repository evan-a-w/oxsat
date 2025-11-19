open! Core

include struct
  open Ds
  module Bitset = Bitset
  module Vec = Vec
  module Pool = Pool
  module Pool_intf = Pool_intf
  module Ptr = Ptr
  module Rb = Rb
end

include struct
  open Unboxed

  module I64 = struct
    include I64

    module Option = struct
      include Option

      module%template Vec = Vec.Make [@kind bits64] (struct
          include Option

          let create_for_vec () = none ()
        end)
    end

    module%template Vec = Vec.Make [@kind bits64] (struct
        include I64

        let create_for_vec () = #0L
      end)
  end

  module F64 = struct
    include F64

    module Option = struct
      include Option

      module%template Vec = Vec.Make [@kind float64] (struct
          include Option

          let create_for_vec () = none ()
        end)
    end

    module%template Vec = Vec.Make [@kind float64] (struct
        include F64

        let create_for_vec () = #0.
      end)
  end
end

module List : sig
  include module type of List

  val partition_tf_local
    :  'a t @ local
    -> f:('a @ local -> bool) @ local
    -> 'a t * 'a t @ local

  val%template fold_local
    :  'a t @ local
    -> init:('acc : k) @ local
    -> f:(('acc : k) @ local -> 'a @ local -> ('acc : k) @ local) @ local
    -> ('acc : k) @ local
  [@@kind k = (value, bits64, bits64 & bits64)]

  val find_local
    :  'a list @ local
    -> f:('a @ local -> 'b option @ local) @ local
    -> 'b option @ local

  val iter_local : 'a list @ local -> f:('a @ local -> unit) @ local -> unit
  val length_local : 'a list @ local -> int
end = struct
  include List

  let length_local l =
    let rec go acc l =
      match l with
      | [] -> 0
      | _ :: rest -> go (acc + 1) rest
    in
    go 0 l
  ;;

  let partition_tf_local t ~f = exclave_
    let rec go l r rest =
      match rest with
      | [] -> exclave_ l, r
      | x :: rest ->
        if f x then exclave_ go (x :: l) r rest else exclave_ go l (x :: r) rest
    in
    go [] [] t
  ;;

  let%template[@kind k = (value, bits64, bits64 & bits64)] fold_local l ~init ~f
    = exclave_
    let rec go (acc : (_ : k)) rest = exclave_
      match rest with
      | [] -> acc
      | x :: rest -> go (f acc x) rest
    in
    go init l
  ;;

  let find_local l ~f = exclave_
    fold_local l ~init:None ~f:(fun acc x -> exclave_
      match acc with
      | Some _ -> acc
      | None -> f x)
  ;;

  let rec iter_local l ~f =
    match l with
    | [] -> ()
    | x :: xs ->
      f x;
      iter_local xs ~f
  ;;
end

module Array : sig
  include module type of Array

  val%template fold_local
    :  'a t @ local
    -> init:('acc : k) @ local
    -> f:(('acc : k) @ local -> 'a @ local -> ('acc : k) @ local) @ local
    -> ('acc : k) @ local
  [@@kind k = (value, bits64, bits64 & bits64)]
end = struct
  include Array

  let%template[@kind k = (value, bits64, bits64 & bits64)] fold_local t ~init ~f
    = exclave_
    let rec go (acc : (_ : k)) i = exclave_
      if length t = i then acc else go (f acc t.(i)) (i + 1)
    in
    go init 0
  ;;
end
