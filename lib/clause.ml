open! Core
open! Import

module T = struct
  type t = int Vec.Value.t
end

module Pool = Pool.Make [@kind value] (T)
include T

let copy t = Array.copy t

let is_tautology t =
  let open Local_ref.O in
  Vec.Value.sort t ~compare:(fun a b -> Int.compare (Int.abs a) (Int.abs b));
  let seen_taut = Local_ref.create false in
  for i = 1 to Vec.Value.length t - 1 do
    if Vec.Value.get t i = -Vec.Value.get t (i - 1) then seen_taut := true
  done;
  !seen_taut [@nontail]
;;

let is_satisfied t ~assignments =
  Vec.Value.fold t ~init:false ~f:(fun acc literal ->
    acc || Bitset.get (Tf_pair.get assignments (literal > 0)) (Int.abs literal))
;;

let iter_literals t ~(f : _ @ local) =
  Tf_pair.iteri t ~f:(fun ~key:value ~data:bitset ->
    Bitset.iter_set_bits bitset ~f:(fun var -> Literal.create ~var ~value |> f)
    [@nontail])
  [@nontail]
;;

let%template literals_list (t : t) : int list @ m =
  let rec go i acc =
    if i = Vec.Value.length t then acc else go (i + 1) (Vec.Value.get t i :: acc)
  in
  let res = go 0 [] in
  (List.rev [@alloc a]) res [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let contains t ~var =
  let open Local_ref.O in
  let seen = Local_ref.create false in
  Vec.Value.iter t ~f:(fun literal ->
    if Int.abs literal = var then seen := true);
  !seen [@nontail]
;;

let contains_literal t ~literal =
  Array.exists t ~f:(fun literal' -> literal = literal') [@nontail]
;;

let unit_literal t ~assignments =
  if is_satisfied t ~assignments
  then Literal.Option.none ()
  else (
    let res =
      Array.fold_local t ~init:(Ok None) ~f:(fun acc literal -> exclave_
        match acc with
        | Error _ -> acc
        | Ok acc ->
          if Bitset.get
               (Tf_pair.get assignments (literal < 0))
               (Int.abs literal)
          then Ok acc
          else (
            match acc with
            | None -> Ok (Some literal)
            | Some _ -> Error ()))
    in
    match res with
    | Ok (Some literal) -> Literal.Option.some (Literal.of_int literal)
    | Ok None | Error _ -> Literal.Option.none ())
;;

let value_exn t ~var =
  if Bitset.get t.#Tf_pair.t var
  then true
  else if Bitset.get t.#f var
  then false
  else failwith "not in clause"
;;

let clear t = Tf_pair.iter t ~f:Bitset.clear_all

let can_resolve t ~other ~on_var =
  let t_pos = Bitset.get t.#Tf_pair.t on_var in
  let t_neg = Bitset.get t.#f on_var in
  let other_pos = Bitset.get other.#Tf_pair.t on_var in
  let other_neg = Bitset.get other.#f on_var in
  (t_pos && other_neg) || (t_neg && other_pos)
;;

let of_int_array t = t

let to_int_array t =
  let v = Vec.Value.create () in
  iter_literals t ~f:(fun literal -> Vec.Value.push v (Literal.to_int literal));
  Vec.Value.to_array v
;;

let resolve_exn t ~other ~on_var =
  if not (can_resolve t ~other ~on_var)
  then
    Error.raise_s
      [%message
        "Can't resolve clauses"
          (on_var : int)
          ~t:(to_int_array t : int array)
          ~other:(to_int_array other : int array)]
  else (
    Bitset.lor_inplace ~dest:t.#Tf_pair.t t.#t other.#Tf_pair.t;
    Bitset.lor_inplace ~dest:t.#f t.#f other.#f;
    Bitset.clear t.#t on_var;
    Bitset.clear t.#f on_var)
;;
