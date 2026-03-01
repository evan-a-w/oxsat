open! Core
open Rb_intf

module Make (Key : Key) (Value : Value) = struct
  module Key = Key
  module Value = Value

  module M = Stdlib.Map.Make (struct
      type t = Key.t

      let compare = Key.compare
    end)

  type t = { mutable map : Value.t M.t }

  module Kv_option = struct
    type t = (Key.t * Value.t) option

    let none () = None
    let some kv = Some kv
    let is_none = Option.is_none
    let is_some = Option.is_some
    let value t ~default = Option.value t ~default

    let value_exn = function
      | Some kv -> kv
      | None -> failwith "Rb.Kv_option.value_exn on none"
    ;;

    module Optional_syntax = struct
      module Optional_syntax = struct
        let is_none = is_none
        let unsafe_value = value_exn
      end
    end
  end

  let create () = { map = M.empty }
  let insert t ~key ~data = t.map <- M.add key data t.map
  let mem t key = M.mem key t.map
  let remove t key = t.map <- M.remove key t.map
  let iter t ~f = M.iter (fun key data -> f ~key ~data) t.map
  let iteri = iter

  let find t key =
    match M.find_opt key t.map with
    | None -> Kv_option.none ()
    | Some data -> Kv_option.some (key, data)
  ;;

  let min t = M.min_binding_opt t.map
  let max t = M.max_binding_opt t.map

  let pop_min t =
    match min t with
    | None -> None
    | Some (key, _data) as res ->
      t.map <- M.remove key t.map;
      res
  ;;

  let pop_max t =
    match max t with
    | None -> None
    | Some (key, _data) as res ->
      t.map <- M.remove key t.map;
      res
  ;;

  let find_exn t key = M.find key t.map

  let min_exn t =
    match min t with
    | Some kv -> kv
    | None -> failwith "Rb.min_exn on empty tree"
  ;;

  let max_exn t =
    match max t with
    | Some kv -> kv
    | None -> failwith "Rb.max_exn on empty tree"
  ;;

  let pop_min_exn t =
    match pop_min t with
    | Some kv -> kv
    | None -> failwith "Rb.pop_min_exn on empty tree"
  ;;

  let pop_max_exn t =
    match pop_max t with
    | Some kv -> kv
    | None -> failwith "Rb.pop_max_exn on empty tree"
  ;;

  let fold t ~init ~f = M.fold (fun key data acc -> f ~acc ~key ~data) t.map init

  let fold_or_null t ~init ~f =
    let done_ = ref false in
    fold t ~init ~f:(fun ~acc ~key ~data ->
      if !done_ then acc else f ~done_ ~acc ~key ~data)
  ;;

  let length t = M.cardinal t.map
  let is_empty t = M.is_empty t.map
  let clear t = t.map <- M.empty

  let to_array t = M.bindings t.map |> Array.of_list
  let to_keys_array t = M.bindings t.map |> List.map ~f:fst |> Array.of_list

  let of_array_exn arr =
    let t = create () in
    Array.iter arr ~f:(fun (key, data) ->
      if mem t key
      then failwith "Rb.of_array_exn: duplicate key"
      else insert t ~key ~data);
    t
  ;;

  let validate _ = ()

  module Iter = struct
    type t = { mutable rest : (Key.t * Value.t) list }

    let create tree = { rest = M.bindings tree.map }

    let create_from tree start =
      { rest = List.filter (M.bindings tree.map) ~f:(fun (key, _) -> Key.compare key start >= 0) }
    ;;

    let next t =
      match t.rest with
      | [] -> Kv_option.none ()
      | hd :: tl ->
        t.rest <- tl;
        Kv_option.some hd
    ;;

    let peek t =
      match t.rest with
      | [] -> Kv_option.none ()
      | hd :: _ -> Kv_option.some hd
    ;;

    let is_done t = List.is_empty t.rest
  end
end
