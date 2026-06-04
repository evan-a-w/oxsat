open! Core
open! Feel
open! Ds
include Atom_registry_intf

module Make (Atom : Atom) (Data : Data) = struct
  type t =
    { atom_to_var : int Atom.Table.t
    ; var_to_atom : Atom.t option Vec.Value.t
    ; atom_to_data : Data.t Atom.Table.t
    }

  let create () =
    let t =
      { atom_to_var = Atom.Table.create ()
      ; var_to_atom = Vec.Value.create ()
      ; atom_to_data = Atom.Table.create ()
      }
    in
    Vec.Value.push t.var_to_atom None;
    t
  ;;

  let register t ~atom ~data =
    let var = Vec.Value.length t.var_to_atom in
    Vec.Value.push t.var_to_atom (Some atom);
    Hashtbl.set t.atom_to_var ~key:atom ~data:var;
    Hashtbl.set t.atom_to_data ~key:atom ~data;
    var
  ;;

  let on_new_var t var =
    if var < Vec.Value.length t.var_to_atom
    then ()
    else Vec.Value.fill_to_length t.var_to_atom ~length:(var + 1) ~f:(fun _ -> None)
  ;;

  let var t ~atom =
    match Hashtbl.find t.atom_to_var atom with
    | None -> Null
    | Some v -> This v
  ;;

  let atom t ~var =
    if var >= Vec.Value.length t.var_to_atom
    then Null
    else (
      match Vec.Value.get t.var_to_atom var with
      | None -> Null
      | Some a -> This a)
  ;;

  let data t ~atom =
    match Hashtbl.find t.atom_to_data atom with
    | None -> None
    | Some d -> Some d
  ;;
end
