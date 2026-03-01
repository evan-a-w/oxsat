open! Core
open Rb_intf

module Make (Key : Key) (Value : Value) = struct
  module Key = Key
  module Value = Value

  type t = { mutable map : (Key.t, Value.t) Map.Poly.t }

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

  let create () = { map = Map.Poly.empty }
  let insert t ~key ~data = t.map <- Map.Poly.set t.map ~key ~data
  let mem t key = Map.Poly.mem t.map key
  let remove t key = t.map <- Map.Poly.remove t.map key
  let iter t ~f = Map.Poly.iteri t.map ~f:(fun ~key ~data -> f ~key ~data)
  let iteri = iter

  let find t key =
    match Map.Poly.find t.map key with
    | None -> Kv_option.none ()
    | Some data -> Kv_option.some (key, data)
  ;;

  let min t = Map.Poly.min_elt t.map
  let max t = Map.Poly.max_elt t.map

  let pop_min t =
    match min t with
    | None -> None
    | Some (key, _data) as res ->
      t.map <- Map.Poly.remove t.map key;
      res
  ;;

  let pop_max t =
    match max t with
    | None -> None
    | Some (key, _data) as res ->
      t.map <- Map.Poly.remove t.map key;
      res
  ;;

  let find_exn t key = Map.Poly.find_exn t.map key

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

  let fold t ~init ~f =
    Map.Poly.fold t.map ~init ~f:(fun ~key ~data acc -> f ~acc ~key ~data)
  ;;

  let fold_or_null t ~init ~f =
    let done_ = ref false in
    fold t ~init ~f:(fun ~acc ~key ~data ->
      if !done_ then acc else f ~done_ ~acc ~key ~data)
  ;;

  let length t = Map.Poly.length t.map
  let is_empty t = Map.Poly.is_empty t.map
  let clear t = t.map <- Map.Poly.empty

  let to_array t = Map.Poly.to_alist t.map |> Array.of_list
  let to_keys_array t = Map.Poly.keys t.map |> Array.of_list

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

    let create tree = { rest = Map.Poly.to_alist tree.map }

    let create_from tree start =
      { rest =
          List.filter (Map.Poly.to_alist tree.map) ~f:(fun (key, _) ->
            Key.compare key start >= 0)
      }
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
