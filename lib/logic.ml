open! Core

let implies a b = [| [| -a; b |] |]
let iff a b = [| [| -a; b |]; [| -b; a |] |]
let xor a b = iff a (-b)

let at_most_one variables =
  [| [| -variables.(i); -variables.(j) |]
     for i = 0 to Array.length variables - 1
     for j = i + 1 to Array.length variables - 1
  |]
;;

let exactly_one variables =
  Array.concat
    [ [| variables |]
    ; [| [| -variables.(i); -variables.(j) |]
         for i = 0 to Array.length variables - 1
         for j = i + 1 to Array.length variables - 1
      |]
    ]
;;
