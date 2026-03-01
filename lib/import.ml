open! Core

module Bitset = Ds.Bitset
module Vec = Ds.Vec
module Pool = Ds.Pool
module Pool_intf = Ds.Pool_intf
module Local_ref = Ds.Local_ref
module Ptr = Ds.Ptr
module Rb = Ds.Rb
module Hash_table = Ds.Hash_table

module I64 = struct
  include Stdlib.Int64

  let to_int_trunc = to_int

  module O = struct
    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mul
    let ( / ) = div
    let ( = ) a b = compare a b = 0
    let ( <> ) a b = compare a b <> 0
    let ( < ) a b = compare a b < 0
    let ( <= ) a b = compare a b <= 0
    let ( > ) a b = compare a b > 0
    let ( >= ) a b = compare a b >= 0
  end

  module Option = struct
    type value = int64
    type t = value option

    let none () = None
    let some x = Some x
    let value t ~default = Option.value t ~default

    module Vec = struct
      type t = value option Vec.Value.t

      let create = Vec.Value.create
      let get = Vec.Value.get
      let set = Vec.Value.set
      let fill_to_length = Vec.Value.fill_to_length
    end
  end

  module Vec = Vec.Value
end

module F64 = struct
  include Float

  module O = struct
    let ( + ) = ( +. )
    let ( - ) = ( -. )
    let ( * ) = ( *. )
    let ( / ) = ( /. )
    let ( = ) (a : float) b = Float.equal a b
    let ( <> ) (a : float) b = not (Float.equal a b)
    let ( < ) (a : float) b = a < b
    let ( <= ) (a : float) b = a <= b
    let ( > ) (a : float) b = a > b
    let ( >= ) (a : float) b = a >= b
  end

  module Option = struct
    type value = float
    type t = value option

    let none () = None
    let some x = Some x
    let value_exn = Option.value_exn
    let compare = Option.compare Float.compare

    module Vec = struct
      type t = value option Vec.Value.t

      let create = Vec.Value.create
      let get = Vec.Value.get
      let set = Vec.Value.set
      let iteri = Vec.Value.iteri
      let push = Vec.Value.push
    end
  end

  module Vec = Vec.Value
end

module List = struct
  include List

  let partition_tf_local = List.partition_tf
  let fold_local = List.fold
  let find_local l ~f = List.find_map l ~f
  let iter_local = List.iter
  let length_local = List.length
end

module Array = struct
  include Array

  let fold_local = Array.fold
end

module Int = struct
  include Int

  module H_set = struct
    type t = unit Int.Table.t

    module Kv_option = struct
      type t = (int * unit) option
    end

    let create () = Int.Table.create ()
    let clear = Hashtbl.clear
    let insert t ~key ~data = Hashtbl.set t ~key ~data
    let remove t key = Hashtbl.remove t key
    let mem t key = Hashtbl.mem t key
    let choose_arbitrarily t = Hashtbl.to_alist t |> List.hd
    let to_keys_array t = Hashtbl.keys t |> Array.of_list
  end
end
