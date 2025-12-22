open! Core
open! Unboxed
open Pool_intf

module type%template
  [@kind
    k
    = ( value
      , bits64
      , value & value
      , bits64 & bits64
      , bits64 & bits64 & bits64
      , value & value & bits64 & bits64 & bits64 & value
      , value & value & value & value & value & value
      , value & bits64 & bits64 & bits64 & bits64 & value
      , bits64 & value & bits64 & bits64 & bits64 & value
      , bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & (bits64 & bits64) & bits64 & bits64 & value
      , bits64 & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , (bits64 & float64)
        & (bits64 & bits64)
        & bits64
        & bits64
        & bits64
        & value
      , value & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & value
      , bits64 & float64 & bits64
      , bits64 & float64 & bits64 & bits64
      , (bits64 & float64 & value) & bits64 & bits64 & bits64 & value
      , (bits64 & float64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & bits64 & bits64 & bits64 & bits64 & value )] S = S
[@kind k]

module%template
  [@kind
    k
    = ( value
      , bits64
      , value & value
      , bits64 & bits64
      , bits64 & bits64 & bits64
      , value & value & bits64 & bits64 & bits64 & value
      , value & value & value & value & value & value
      , value & bits64 & bits64 & bits64 & bits64 & value
      , bits64 & value & bits64 & bits64 & bits64 & value
      , bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64)
        & (bits64 & bits64)
        & bits64
        & bits64
        & bits64
        & value
      , (bits64 & bits64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & bits64 & bits64 & bits64 & bits64 & value
      , bits64 & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , value & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & value
      , bits64 & float64 & bits64
      , bits64 & float64 & bits64 & bits64
      , (bits64 & float64 & value) & bits64 & bits64 & bits64 & value
      , (bits64 & float64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & (bits64 & bits64) & bits64 & bits64 & value )] Make
    (Arg : Elt
  [@kind k]) =
struct
  module Elt = Arg

  module Chunk = struct
    module Chunk_elt = struct
      type t =
        #{ next_free : int or_null
         ; next_taken : int or_null
         ; elt : Elt.t
         }

      let create ~next_free ~next_taken ~elt = #{ next_free; next_taken; elt }
    end

    type t =
      { mutable next_free_chunk : int or_null
      ; mutable first_free : int or_null
      ; mutable first_taken : int or_null
      ; elts : Chunk_elt.t array
      }

    let create ~next_free_chunk ~chunk_size =
      let elts =
        Array.create
          ~len:chunk_size
          (Chunk_elt.create
             ~next_free:Null
             ~next_taken:Null
             ~elt:(Elt.create_for_pool ()))
      in
      for i = 0 to pred (pred chunk_size) do
        elts.(i) <- #{ (elts.(i)) with next_free = This (i + 1) }
      done;
      (* Last element points to itself to indicate end of free list *)
      elts.(pred chunk_size)
      <- #{ (elts.(pred chunk_size)) with next_free = This (pred chunk_size) };
      { next_free_chunk; first_free = This 0; first_taken = Null; elts }
    ;;

    let is_fully_allocated t = Or_null.is_null t.first_free

    let alloc t =
      match t.first_free with
      | Null -> failwith "bug"
      | This idx ->
        let next = t.elts.(idx).#next_free in
        t.first_free
        <- (match next with
            | This i when i = idx -> Null
            | _ -> next);
        (* Add to taken list *)
        t.elts.(idx)
        <- #{ (t.elts.(idx)) with next_free = Null; next_taken = t.first_taken };
        t.first_taken <- This idx;
        idx
    ;;

    let free t idx =
      (* Remove from taken list *)
      let rec remove_from_taken prev curr =
        match curr with
        | Null -> ()
        | This i when i = idx ->
          (match prev with
           | Null -> t.first_taken <- t.elts.(i).#next_taken
           | This prev_i ->
             t.elts.(prev_i)
             <- #{ (t.elts.(prev_i)) with next_taken = t.elts.(i).#next_taken })
        | This i -> remove_from_taken curr t.elts.(i).#next_taken
      in
      remove_from_taken Null t.first_taken;
      (* Add to free list *)
      t.elts.(idx)
      <- #{ (t.elts.(idx)) with
            next_free =
              (match t.first_free with
               | Null -> This idx
               | _ -> t.first_free)
          ; next_taken = Null
          };
      t.first_free <- This idx
    ;;
  end

  type t =
    { mutable first_free : int or_null
    ; mutable outstanding : int
    ; chunk_size : int
    ; chunks : Chunk.t Vec.Value.t
    }

  let create ?(chunk_size = 4096) () =
    (* chunk_size is a power of 2 *)
    assert (chunk_size > 0 && chunk_size land (chunk_size - 1) = 0);
    { first_free = Null
    ; chunk_size
    ; chunks = Vec.Value.create ()
    ; outstanding = 0
    }
  ;;

  let chunk_bits t = Int.floor_log2 t.chunk_size

  module Ptr_helpers = struct
    let chunk t ptr = Ptr.Private.chunk ~chunk_bits:(chunk_bits t) ptr
    let idx t ptr = Ptr.Private.idx ~chunk_bits:(chunk_bits t) ptr

    let create t ~chunk ~idx =
      Ptr.Private.create ~chunk_bits:(chunk_bits t) ~chunk ~idx
    ;;
  end

  let outstanding t = t.outstanding

  let get t ptr =
    (Vec.Value.get t.chunks (Ptr_helpers.chunk t ptr)).elts.(Ptr_helpers.idx
                                                               t
                                                               ptr)
      .#elt
  ;;

  let set t ptr elt =
    let chunk_elt =
      (Vec.Value.get t.chunks (Ptr_helpers.chunk t ptr)).elts.(Ptr_helpers.idx
                                                                 t
                                                                 ptr)
    in
    (Vec.Value.get t.chunks (Ptr_helpers.chunk t ptr)).elts.(Ptr_helpers.idx
                                                               t
                                                               ptr)
    <- #{ chunk_elt with elt }
  ;;

  let rec alloc t =
    t.outstanding <- t.outstanding + 1;
    match t.first_free with
    | Null ->
      let new_idx = Vec.Value.length t.chunks in
      Vec.Value.push
        t.chunks
        (Chunk.create ~chunk_size:t.chunk_size ~next_free_chunk:Null);
      t.first_free <- This new_idx;
      alloc t
    | This idx ->
      let chunk = Vec.Value.get t.chunks idx in
      let idx' = Chunk.alloc chunk in
      if Chunk.is_fully_allocated chunk
      then t.first_free <- chunk.next_free_chunk;
      Ptr_helpers.create t ~chunk:idx ~idx:idx'
  ;;

  let free t ptr =
    t.outstanding <- t.outstanding - 1;
    let c = Vec.Value.get t.chunks (Ptr_helpers.chunk t ptr) in
    let i = Ptr_helpers.idx t ptr in
    let previously_full = Chunk.is_fully_allocated c in
    Chunk.free c i;
    if previously_full
    then (
      c.next_free_chunk <- t.first_free;
      t.first_free <- This (Ptr_helpers.chunk t ptr))
  ;;

  let iter t ~f =
    for chunk_idx = 0 to Vec.Value.length t.chunks - 1 do
      let chunk = Vec.Value.get t.chunks chunk_idx in
      (* Follow the taken list *)
      let rec iter_taken next =
        match next with
        | Null -> ()
        | This idx ->
          f (Ptr_helpers.create t ~chunk:chunk_idx ~idx);
          iter_taken chunk.elts.(idx).#next_taken
      in
      iter_taken chunk.first_taken
    done
  ;;
end

module%template
  [@kind
    k
    = ( bits64
      , value & value
      , bits64 & bits64
      , bits64 & bits64 & bits64
      , value & value & bits64 & bits64 & bits64 & value
      , value & value & value & value & value & value
      , value & bits64 & bits64 & bits64 & bits64 & value
      , bits64 & value & bits64 & bits64 & bits64 & value
      , bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , (bits64 & bits64) & bits64 & bits64 & bits64 & bits64 & value
      , bits64 & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , value & (bits64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & value
      , bits64 & float64 & bits64
      , bits64 & float64 & bits64 & bits64
      , (bits64 & float64 & value) & bits64 & bits64 & bits64 & value
      , (bits64 & float64 & bits64) & bits64 & bits64 & bits64 & value
      , bits64 & float64 & bits64 & bits64 & bits64 & bits64 & bits64 & value
      , (bits64 & float64)
        & (bits64 & bits64)
        & bits64
        & bits64
        & bits64
        & value
      , (bits64 & float64) & value & bits64 & bits64 & bits64 & value
      , (bits64 & float64) & (bits64 & bits64) & bits64 & bits64 & value
      , (bits64 & float64) & bits64 & bits64 & bits64 & bits64 & value )] Make_global
    (Arg : Elt
  [@kind k]) =
struct
  include Make [@kind k] (Arg)

  let global = create ()
  let alloc () = alloc global
  let free ptr = free global ptr
  let get ptr = get global ptr
  let set ptr value = set global ptr value
  let iter ~f = iter global ~f
end
