open! Core
open! Feel.Import

type t =
  { term_by_tvar : Formula.any Tvar.Table.t
  ; injected : Tvar.Hash_set.t
  }

let create () =
  { term_by_tvar = Tvar.Table.create (); injected = Tvar.Hash_set.create () }
;;

let register t ~tvar ~term = Hashtbl.set t.term_by_tvar ~key:tvar ~data:term
let mem t tvar = Hashtbl.mem t.term_by_tvar tvar

let maybe_get_lemma t =
  Hashtbl.to_alist t.term_by_tvar
  |> List.find ~f:(fun (tvar, _) -> not (Hash_set.mem t.injected tvar))
  |> Option.map ~f:(fun (tvar, term) ->
    Hash_set.add t.injected tvar;
    `Lemma [ `Eq (term, Formula.Var tvar), true ])
  |> Option.value ~default:`Consistent
;;
