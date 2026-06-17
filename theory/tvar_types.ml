open! Core

module Type = struct
  type t =
    | Int
    | Float
  [@@deriving sexp, compare, equal]
end

type trail_entry =
  { depth : int
  ; var : Tvar.t
  ; old_type : Type.t option
  }

type t =
  { types : Type.t Tvar.Table.t
  ; trail : trail_entry list ref
  ; mutable depth : int
  }

let create () = { types = Tvar.Table.create (); trail = ref []; depth = 0 }
let push t = t.depth <- t.depth + 1

let pop t =
  t.depth <- t.depth - 1;
  let rec undo () =
    match !(t.trail) with
    | entry :: rest when entry.depth > t.depth ->
      t.trail := rest;
      (match entry.old_type with
       | None -> Hashtbl.remove t.types entry.var
       | Some typ -> Hashtbl.set t.types ~key:entry.var ~data:typ);
      undo ()
    | _ -> ()
  in
  undo ()
;;

let assert_type t var typ =
  let old_type = Hashtbl.find t.types var in
  t.trail := { depth = t.depth; var; old_type } :: !(t.trail);
  Hashtbl.set t.types ~key:var ~data:typ
;;

let get_type t var = Hashtbl.find t.types var
