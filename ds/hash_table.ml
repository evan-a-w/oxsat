open! Core
open Hash_table_intf

module Make (Key : Key) (Value : Value) = struct
  module Key = Key
  module Value = Value

  type bucket = (Key.t * Value.t) list

  type t =
    { buckets : bucket Int.Table.t
    ; mutable length : int
    }

  module Kv_option = struct
    type t = (Key.t * Value.t) option

    let none () = None
    let some kv = Some kv
    let is_none = Option.is_none
    let is_some = Option.is_some
    let value t ~default = Option.value t ~default

    let value_exn = function
      | Some kv -> kv
      | None -> failwith "Hash_table.Kv_option.value_exn on none"
    ;;

    module Optional_syntax = struct
      module Optional_syntax = struct
        let is_none = is_none
        let unsafe_value = value_exn
      end
    end
  end

  let create ?capacity ?max_load_percent:_ () =
    let size = Option.value capacity ~default:16 |> Int.max 1 in
    { buckets = Int.Table.create ~size (); length = 0 }
  ;;

  let length t = t.length
  let is_empty t = Int.equal t.length 0

  let load_factor t =
    let bucket_count = Hashtbl.length t.buckets in
    if Int.equal bucket_count 0
    then 0.
    else Float.of_int t.length /. Float.of_int bucket_count
  ;;

  let clear t =
    Hashtbl.clear t.buckets;
    t.length <- 0
  ;;

  let find_in_bucket bucket key =
    List.find_map bucket ~f:(fun (k, v) -> if Key.equal k key then Some (k, v) else None)
  ;;

  let update_bucket t hash ~f =
    let old_bucket = Hashtbl.find t.buckets hash |> Option.value ~default:[] in
    let new_bucket = f old_bucket in
    if List.is_empty new_bucket
    then Hashtbl.remove t.buckets hash
    else Hashtbl.set t.buckets ~key:hash ~data:new_bucket
  ;;

  let insert t ~key ~data =
    let hash = Key.hash key in
    let inserted_new = ref true in
    update_bucket t hash ~f:(fun bucket ->
      let rec go acc = function
        | [] -> List.rev ((key, data) :: acc)
        | (k, _) :: tl when Key.equal k key ->
          inserted_new := false;
          List.rev_append acc ((k, data) :: tl)
        | pair :: tl -> go (pair :: acc) tl
      in
      go [] bucket);
    if !inserted_new then t.length <- t.length + 1
  ;;

  let mem t key =
    let hash = Key.hash key in
    let bucket = Hashtbl.find t.buckets hash |> Option.value ~default:[] in
    Option.is_some (find_in_bucket bucket key)
  ;;

  let remove t key =
    let hash = Key.hash key in
    let removed = ref false in
    update_bucket t hash ~f:(fun bucket ->
      let rec go acc = function
        | [] -> List.rev acc
        | (k, _) :: tl when Key.equal k key ->
          removed := true;
          List.rev_append acc tl
        | pair :: tl -> go (pair :: acc) tl
      in
      go [] bucket);
    if !removed then t.length <- t.length - 1
  ;;

  let find t key =
    let hash = Key.hash key in
    let bucket = Hashtbl.find t.buckets hash |> Option.value ~default:[] in
    find_in_bucket bucket key
  ;;

  let find_exn t key =
    match find t key with
    | Some (_, v) -> v
    | None -> failwith "Hash_table.find_exn: key not found"
  ;;

  let iter t ~f =
    Hashtbl.iteri t.buckets ~f:(fun ~key:_ ~data:bucket ->
      List.iter bucket ~f:(fun (key, data) -> f ~key ~data))
  ;;

  let iteri = iter

  let fold t ~init ~f =
    let acc = ref init in
    iter t ~f:(fun ~key ~data -> acc := f ~acc:!acc ~key ~data);
    !acc
  ;;

  let to_array t =
    fold t ~init:[] ~f:(fun ~acc ~key ~data -> (key, data) :: acc)
    |> List.rev
    |> Array.of_list
  ;;

  let to_keys_array t = to_array t |> Array.map ~f:fst

  let choose_arbitrarily t =
    let found = ref None in
    if not (is_empty t)
    then
      Hashtbl.iteri t.buckets ~f:(fun ~key:_ ~data:bucket ->
        match !found, bucket with
        | Some _, _ -> ()
        | None, pair :: _ -> found := Some pair
        | None, [] -> ());
    !found
  ;;
end
