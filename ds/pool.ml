open! Core
open Pool_intf

module type S = Pool_intf.S

module Make (Arg : Elt) = struct
  module Elt = Arg

  type t =
    { mutable cells : Elt.t option array
    ; mutable next : int
    ; mutable free_list : int list
    ; mutable outstanding_count : int
    ; chunk_size : int
    }

  let create ?(chunk_size = 256) () =
    let chunk_size = Int.max 1 chunk_size in
    { cells = Array.create ~len:chunk_size None
    ; next = 0
    ; free_list = []
    ; outstanding_count = 0
    ; chunk_size
    }
  ;;

  let grow t =
    let old_len = Array.length t.cells in
    let new_cells = Array.create ~len:(old_len + t.chunk_size) None in
    Array.blit ~src:t.cells ~src_pos:0 ~dst:new_cells ~dst_pos:0 ~len:old_len;
    t.cells <- new_cells
  ;;

  let alloc t =
    let idx =
      match t.free_list with
      | i :: rest ->
        t.free_list <- rest;
        i
      | [] ->
        if t.next >= Array.length t.cells then grow t;
        let i = t.next in
        t.next <- t.next + 1;
        i
    in
    t.cells.(idx) <- Some (Elt.create_for_pool ());
    t.outstanding_count <- t.outstanding_count + 1;
    Ptr.of_int idx
  ;;

  let free t ptr =
    let idx = Ptr.to_int ptr in
    if idx < 0 || idx >= t.next
    then invalid_arg "Pool.free: invalid pointer"
    else
      match t.cells.(idx) with
      | None -> invalid_arg "Pool.free: pointer already freed"
      | Some _ ->
        t.cells.(idx) <- None;
        t.free_list <- idx :: t.free_list;
        t.outstanding_count <- t.outstanding_count - 1
  ;;

  let get t ptr =
    let idx = Ptr.to_int ptr in
    if idx < 0 || idx >= t.next
    then invalid_arg "Pool.get: invalid pointer"
    else
      match t.cells.(idx) with
      | Some v -> v
      | None -> failwith "Pool.get: pointer freed"
  ;;

  let set t ptr value =
    let idx = Ptr.to_int ptr in
    if idx < 0 || idx >= t.next
    then invalid_arg "Pool.set: invalid pointer"
    else
      match t.cells.(idx) with
      | None -> failwith "Pool.set: pointer freed"
      | Some _ -> t.cells.(idx) <- Some value
  ;;

  let iter t ~f =
    for i = 0 to t.next - 1 do
      match t.cells.(i) with
      | Some _ -> f (Ptr.of_int i)
      | None -> ()
    done
  ;;

  let outstanding t = t.outstanding_count
end

module Make_global (Arg : Elt) = struct
  module Elt = Arg
  module P = Make (Arg)

  let pool = P.create ()
  let alloc () = P.alloc pool
  let free ptr = P.free pool ptr
  let get ptr = P.get pool ptr
  let set ptr value = P.set pool ptr value
  let iter ~f = P.iter pool ~f
end
