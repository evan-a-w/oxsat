type _ t =
  | Var : string -> _ t
  | Add : 'a t * 'a t -> ([> `La ] as 'a) t
  | Mul_const : int * 'a t -> ([> `La ] as 'a) t
  | App : string * 'a t list -> ([> `Uf ] as 'a) t
  | Scale_const : int * 'a t -> ([> `La ] as 'a) t

(* Scale_const should accept an argument of ANY tag set (e.g. pure `Uf,
   or `La, or combined), but the result must be at least `La. *)
let scaled_uf : [ `La | `Uf ] t = Scale_const (2, App ("f", [ Var "x" ]))
let scaled_la : [ `La ] t = Scale_const (2, Var "x")
let scaled_any : [ `La | `Uf ] t = Scale_const (2, Add (Var "x", Var "y"))

(* This should be rejected: result claims only `La but arg is pure `Uf-only term *)
(* let bad : [ `La ] t = Scale_const (2, App ("f", [ Var "x" ])) *)

let () = ignore (scaled_uf, scaled_la, scaled_any)
