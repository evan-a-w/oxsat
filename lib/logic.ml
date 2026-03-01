open! Core

let implies a b = [| [| -a; b |] |]
let iff a b = [| [| -a; b |]; [| -b; a |] |]
let xor a b = iff a (-b)

let at_most_one variables =
  let n = Array.length variables in
  let out = ref [] in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      out := [| -variables.(i); -variables.(j) |] :: !out
    done
  done;
  List.rev !out |> Array.of_list
;;

let exactly_one variables = Array.append [| variables |] (at_most_one variables)
