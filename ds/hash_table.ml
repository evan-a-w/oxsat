open! Core
open! Unboxed
open Hash_table_intf

module%template
  [@kind
    k = (value, bits64, bits64 & bits64, bits64 & float64)
    , v = (value, bits64, bits64 & bits64)] Make
    (Key : Key
  [@kind k])
    (Value : Value
  [@kind v]) =
struct
  module Key = Key
  module Value = Value

  type slot_state : immediate mod external_ =
    | Empty
    | Tombstone
    | Occupied

  module Slot = struct
    type t =
      #{ key : Key.t
       ; data : Value.t
       ; state : slot_state
       }

    let[@inline] create ~key ~data ~state = #{ key; data; state }

    let[@inline] empty () =
      create
        ~state:Empty
        ~key:(Key.create_for_hash_table ())
        ~data:(Value.create_for_hash_table ())
    ;;

    let[@inline] occupied ~key ~data = create ~state:Occupied ~key ~data
  end

  let default_capacity = 16
  let default_max_load_percent = 75

  type t =
    { mutable slots : Slot.t array
    ; mutable mask : int
    ; mutable length : int
    ; mutable tombstones : int
    ; mutable first_occupied : int
    ; max_load_percent : int
    }

  let[@inline] capacity t = Array.length t.slots

  let[@inline] create_slot_array capacity =
    Array.create ~len:capacity (Slot.empty ())
  ;;

  let next_power_of_two n =
    if n <= 1
    then 1
    else (
      let rec loop pow =
        if pow >= n
        then pow
        else (
          let next = pow lsl 1 in
          if next <= 0 then pow else loop next)
      in
      loop 1)
  ;;

  let[@inline] normalized_capacity cap =
    let cap = Int.max 1 cap in
    next_power_of_two cap
  ;;

  let create
    ?(capacity = default_capacity)
    ?(max_load_percent = default_max_load_percent)
    ()
    =
    let capacity = normalized_capacity capacity in
    let max_load_percent =
      if max_load_percent <= 0 || max_load_percent > 100
      then invalid_arg "max_load_percent must be between 1 and 100"
      else max_load_percent
    in
    { slots = create_slot_array capacity
    ; mask = capacity - 1
    ; length = 0
    ; tombstones = 0
    ; first_occupied = capacity
    ; max_load_percent
    }
  ;;

  let length t = t.length
  let is_empty t = t.length = 0

  let load_factor t =
    if capacity t = 0
    then 0.
    else Float.of_int t.length /. Float.of_int (capacity t)
  ;;

  let clear t =
    let cap = capacity t in
    t.slots <- create_slot_array cap;
    t.mask <- cap - 1;
    t.length <- 0;
    t.tombstones <- 0;
    t.first_occupied <- cap
  ;;

  module Kv_option =
    Optional_pair.Make [@kind k v]
      (struct
        include Key

        let trivial_create_for_none = create_for_hash_table
      end)
      (struct
        include Value

        let trivial_create_for_none = create_for_hash_table
      end)

  let rec find_index_loop t key ~steps ~index ~capacity =
    if steps >= capacity
    then -1
    else (
      let slot = t.slots.(index) in
      match slot.#state with
      | Empty -> -1
      | Tombstone ->
        let next = (index + 1) land t.mask in
        find_index_loop t key ~steps:(steps + 1) ~index:next ~capacity
      | Occupied ->
        if Key.equal key slot.#key
        then index
        else (
          let next = (index + 1) land t.mask in
          find_index_loop t key ~steps:(steps + 1) ~index:next ~capacity))
  ;;

  let find_index t key =
    let hash = Key.hash key in
    find_index_loop
      t
      key
      ~steps:0
      ~index:(hash land t.mask)
      ~capacity:(capacity t)
  ;;

  let find t key =
    let idx = find_index t key in
    if idx < 0
    then Kv_option.none ()
    else (
      let slot = t.slots.(idx) in
      Kv_option.some #(slot.#key, slot.#data))
  ;;

  let find_exn t key =
    let result = find t key in
    if Kv_option.is_none result
    then raise (Not_found_s [%message "Key not found"]);
    let #(_, data) = Kv_option.value_exn result in
    data
  ;;

  let mem t key = Kv_option.is_some (find t key)

  type insert_result =
    | Inserted of int
    | Updated of int
    | Full

  let rec insert_probe t key ~data ~steps ~index ~first_tombstone ~capacity
    = exclave_
    if steps >= capacity
    then Full
    else (
      let slot = t.slots.(index) in
      match slot.#state with
      | Empty ->
        let insert_index =
          if first_tombstone >= 0 then first_tombstone else index
        in
        t.slots.(insert_index) <- Slot.occupied ~key ~data;
        t.length <- t.length + 1;
        if first_tombstone >= 0 then t.tombstones <- t.tombstones - 1;
        Inserted insert_index
      | Tombstone ->
        let first_tombstone =
          if first_tombstone >= 0 then first_tombstone else index
        in
        let next = (index + 1) land t.mask in
        insert_probe
          t
          key
          ~data
          ~steps:(steps + 1)
          ~index:next
          ~first_tombstone
          ~capacity
      | Occupied ->
        if Key.equal key slot.#key
        then (
          t.slots.(index) <- #{ slot with data };
          Updated index)
        else (
          let next = (index + 1) land t.mask in
          insert_probe
            t
            key
            ~data
            ~steps:(steps + 1)
            ~index:next
            ~first_tombstone
            ~capacity))
  ;;

  let insert_into_slots t ~key ~data = exclave_
    let hash = Key.hash key in
    insert_probe
      t
      key
      ~data
      ~steps:0
      ~index:(hash land t.mask)
      ~first_tombstone:(-1)
      ~capacity:(capacity t)
  ;;

  let should_grow t =
    let cap = capacity t in
    cap > 0 && (t.length + 1) * 100 >= cap * t.max_load_percent
  ;;

  let need_cleanup t = t.tombstones > 0 && t.tombstones * 2 > capacity t

  let rehash t new_capacity =
    let old_slots = t.slots in
    let new_capacity = normalized_capacity new_capacity in
    let new_slots = create_slot_array new_capacity in
    t.slots <- new_slots;
    t.mask <- new_capacity - 1;
    t.length <- 0;
    t.tombstones <- 0;
    t.first_occupied <- new_capacity;
    for i = 0 to Array.length old_slots - 1 do
      let slot = old_slots.(i) in
      match slot.#state with
      | Occupied ->
        (match insert_into_slots t ~key:slot.#key ~data:slot.#data with
         | Inserted idx | Updated idx ->
           if idx < t.first_occupied then t.first_occupied <- idx
         | Full -> failwith "rehash failed: table is full")
      | Empty | Tombstone -> ()
    done
  ;;

  let ensure_capacity t =
    if should_grow t
    then rehash t (capacity t lsl 1)
    else if need_cleanup t
    then rehash t (capacity t)
  ;;

  let[@inline] update_first_occupied_on_insert t idx =
    if idx < t.first_occupied then t.first_occupied <- idx
  ;;

  let advance_first_occupied_from t start =
    let cap = capacity t in
    let rec loop i =
      if i >= cap
      then t.first_occupied <- cap
      else (
        match t.slots.(i).#state with
        | Occupied -> t.first_occupied <- i
        | Empty | Tombstone -> loop (i + 1))
    in
    loop start
  ;;

  let[@inline] ensure_first_occupied t =
    let cap = capacity t in
    if t.first_occupied < cap
    then (
      match t.slots.(t.first_occupied).#state with
      | Occupied -> ()
      | Empty | Tombstone -> advance_first_occupied_from t t.first_occupied)
    else if t.length = 0
    then ()
    else advance_first_occupied_from t 0
  ;;

  let insert t ~key ~data =
    ensure_capacity t;
    match insert_into_slots t ~key ~data with
    | Inserted idx -> update_first_occupied_on_insert t idx
    | Updated _ -> ()
    | Full ->
      rehash t (capacity t lsl 1);
      (match insert_into_slots t ~key ~data with
       | Inserted idx | Updated idx -> update_first_occupied_on_insert t idx
       | Full -> failwith "insert failed after rehash")
  ;;

  let remove t key =
    let idx = find_index t key in
    if idx >= 0
    then (
      let slot = t.slots.(idx) in
      t.slots.(idx) <- #{ slot with state = Tombstone };
      t.length <- t.length - 1;
      t.tombstones <- t.tombstones + 1;
      if idx = t.first_occupied then advance_first_occupied_from t (idx + 1))
  ;;

  let iter t ~f =
    for i = 0 to Array.length t.slots - 1 do
      let slot = t.slots.(i) in
      match slot.#state with
      | Occupied -> f ~key:slot.#key ~data:slot.#data
      | Empty | Tombstone -> ()
    done
  ;;

  let iteri = iter

  let fold t ~init ~f =
    let acc = ref init in
    iter t ~f:(fun ~key ~data -> acc := f ~acc:!acc ~key ~data);
    !acc
  ;;

  let%template to_array t : #(Key.t * Value.t) array @ m =
    let len = length t in
    if len = 0
    then [||]
    else (
      (let arr =
         (Array.create [@alloc a])
           ~len
           #(Key.create_for_hash_table (), Value.create_for_hash_table ())
       in
       let idx = ref 0 in
       iter t ~f:(fun ~key ~data ->
         arr.(!idx) <- #(key, data);
         incr idx);
       arr)
      [@exclave_if_stack a])
  [@@alloc a @ m = (stack_local, heap_global)]
  ;;

  let%template to_keys_array t : Key.t array @ m =
    let len = length t in
    if len = 0
    then [||]
    else (
      (let arr =
         (Array.create [@alloc a]) ~len (Key.create_for_hash_table ())
       in
       let idx = ref 0 in
       iter t ~f:(fun ~key ~data:_ ->
         arr.(!idx) <- key;
         incr idx);
       arr)
      [@exclave_if_stack a])
  [@@alloc a @ m = (stack_local, heap_global)]
  ;;

  let choose_arbitrarily t =
    ensure_first_occupied t;
    let cap = capacity t in
    if t.first_occupied >= cap
    then Kv_option.none ()
    else (
      let slot = t.slots.(t.first_occupied) in
      match slot.#state with
      | Occupied -> Kv_option.some #(slot.#key, slot.#data)
      | Empty | Tombstone ->
        advance_first_occupied_from t (t.first_occupied + 1);
        if t.first_occupied >= cap
        then Kv_option.none ()
        else (
          let slot = t.slots.(t.first_occupied) in
          Kv_option.some #(slot.#key, slot.#data)))
  ;;
end
