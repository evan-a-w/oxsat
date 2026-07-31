open! Core
open! Ds

let%expect_test "canonical zero" =
  let zeros =
    [ Big_int.zero
    ; Big_int.of_int 0
    ; Big_int.of_string "-0"
    ; Big_int.of_string "0"
    ; Big_int.of_string "+0"
    ; Big_int.neg Big_int.zero
    ; Big_int.abs Big_int.zero
    ; Big_int.succ (Big_int.of_int (-1))
    ; Big_int.pred Big_int.one
    ; Big_int.(of_int 3 - of_int 3)
    ; Big_int.(of_int 0 * of_int 5)
    ]
  in
  List.iter zeros ~f:(fun z ->
    print_s
      [%message
        (Big_int.to_string z : string)
          ~is_zero:(Big_int.is_zero z : bool)
          ~sign:(Big_int.sign z : [ `Neg | `Zero | `Pos ])
          ~sign':(Big_int.sign' z : int)]);
  print_s
    [%message
      "neg zero"
        ~equal:(Big_int.equal Big_int.zero (Big_int.neg Big_int.zero) : bool)
        ~cmp:(Big_int.compare Big_int.zero (Big_int.neg Big_int.zero) : int)];
  [%expect
    {|
    (("Big_int.to_string z" 0) (is_zero true) (sign Zero) (sign' 0))
    (("Big_int.to_string z" 0) (is_zero true) (sign Zero) (sign' 0))
    (("Big_int.to_string z" 0) (is_zero true) (sign Zero) (sign' 0))
    (("Big_int.to_string z" 0) (is_zero true) (sign Zero) (sign' 0))
    (("Big_int.to_string z" 0) (is_zero true) (sign Zero) (sign' 0))
    (("Big_int.to_string z" 0) (is_zero true) (sign Zero) (sign' 0))
    (("Big_int.to_string z" 0) (is_zero true) (sign Zero) (sign' 0))
    (("Big_int.to_string z" 0) (is_zero true) (sign Zero) (sign' 0))
    (("Big_int.to_string z" 0) (is_zero true) (sign Zero) (sign' 0))
    (("Big_int.to_string z" 0) (is_zero true) (sign Zero) (sign' 0))
    (("Big_int.to_string z" 0) (is_zero true) (sign Zero) (sign' 0))
    ("neg zero" (equal true) (cmp 0))
    |}]
;;

let%expect_test "parse and print round trip" =
  let values =
    [ "0"
    ; "1"
    ; "-1"
    ; "10"
    ; "-10"
    ; "42"
    ; "-42"
    ; "32767"
    ; "32768"
    ; "65535"
    ; "65536"
    ; "123456789"
    ; "-123456789"
    ; "123456789012345678901234567890"
    ; "-123456789012345678901234567890"
    ; "1000000000000000000000000000000000000000000000000000000000"
    ; "999999999999999999999999999999999999999999999999999999999999"
    ]
  in
  List.iter values ~f:(fun s ->
    print_s
      [%message
        (s : string)
          ~round_trip:(Big_int.to_string (Big_int.of_string s) : string)]);
  let malformed = [ ""; "-"; "+"; "12a3"; " 12"; "12 3" ] in
  List.iter malformed ~f:(fun s ->
    print_s
      [%message
        (s : string)
          ~bomb:
            ((try Big_int.to_string (Big_int.of_string s) with
              | _ -> "<raised>")
             : string)]);
  [%expect
    {|
    ((s 0) (round_trip 0))
    ((s 1) (round_trip 1))
    ((s -1) (round_trip -1))
    ((s 10) (round_trip 10))
    ((s -10) (round_trip -10))
    ((s 42) (round_trip 42))
    ((s -42) (round_trip -42))
    ((s 32767) (round_trip 32767))
    ((s 32768) (round_trip 32768))
    ((s 65535) (round_trip 65535))
    ((s 65536) (round_trip 65536))
    ((s 123456789) (round_trip 123456789))
    ((s -123456789) (round_trip -123456789))
    ((s 123456789012345678901234567890)
     (round_trip 123456789012345678901234567890))
    ((s -123456789012345678901234567890)
     (round_trip -123456789012345678901234567890))
    ((s 1000000000000000000000000000000000000000000000000000000000)
     (round_trip 1000000000000000000000000000000000000000000000000000000000))
    ((s 999999999999999999999999999999999999999999999999999999999999)
     (round_trip 999999999999999999999999999999999999999999999999999999999999))
    ((s "") (bomb <raised>))
    ((s -) (bomb <raised>))
    ((s +) (bomb <raised>))
    ((s 12a3) (bomb <raised>))
    ((s " 12") (bomb <raised>))
    ((s "12 3") (bomb <raised>))
    |}]
;;

let%expect_test "addition and subtraction" =
  let show name t = print_s [%message name (Big_int.to_string t : string)] in
  show "5 + 3" Big_int.(of_int 5 + of_int 3);
  show "5 + -3" Big_int.(of_int 5 + of_int (-3));
  show "-5 + 3" Big_int.(of_int (-5) + of_int 3);
  show "-5 + -3" Big_int.(of_int (-5) + of_int (-3));
  show "3 - 5" Big_int.(of_int 3 - of_int 5);
  show "-3 - -5" Big_int.(of_int (-3) - of_int (-5));
  (* Limb boundaries: base is 2^15. *)
  show "32767 + 1" Big_int.(of_int 32767 + of_int 1);
  show "32768 + 32768" Big_int.(of_int 32768 + of_int 32768);
  show
    "big1 + big2"
    Big_int.(
      of_string "999999999999999999999999999999999"
      + of_string "1000000000000000000000000000000001");
  show
    "big - big"
    Big_int.(
      of_string "123456789012345678901234567890"
      - of_string "98765432109876543210");
  show
    "negative result"
    Big_int.(
      of_string "12345678901234567890" - of_string "123456789012345678901");
  show "succ" Big_int.(succ (of_string "123456789012345678901234567890"));
  show "pred" Big_int.(pred (of_string "-1000000000000000000000000000000"));
  [%expect
    {|
    ("5 + 3" ("Big_int.to_string t" 8))
    ("5 + -3" ("Big_int.to_string t" 2))
    ("-5 + 3" ("Big_int.to_string t" -2))
    ("-5 + -3" ("Big_int.to_string t" -8))
    ("3 - 5" ("Big_int.to_string t" -2))
    ("-3 - -5" ("Big_int.to_string t" 2))
    ("32767 + 1" ("Big_int.to_string t" 32768))
    ("32768 + 32768" ("Big_int.to_string t" 65536))
    ("big1 + big2" ("Big_int.to_string t" 2000000000000000000000000000000000))
    ("big - big" ("Big_int.to_string t" 123456788913580246791358024680))
    ("negative result" ("Big_int.to_string t" -111111110111111111011))
    (succ ("Big_int.to_string t" 123456789012345678901234567891))
    (pred ("Big_int.to_string t" -1000000000000000000000000000001))
    |}]
;;

let%expect_test "multiplication" =
  let show name t = print_s [%message name (Big_int.to_string t : string)] in
  show "6 * 7" Big_int.(of_int 6 * of_int 7);
  show "-6 * 7" Big_int.(of_int (-6) * of_int 7);
  show "6 * -7" Big_int.(of_int 6 * of_int (-7));
  show "-6 * -7" Big_int.(of_int (-6) * of_int (-7));
  show "0 * 5" Big_int.(of_int 0 * of_int 5);
  show "limb boundary" Big_int.(of_int 32768 * of_int 32768);
  show
    "big * big"
    Big_int.(
      of_string "123456789012345678901234567890"
      * of_string "987654321098765432109876543210");
  show
    "big * small"
    Big_int.(of_string "123456789012345678901234567890" * of_int (-1000000));
  [%expect
    {|
    ("6 * 7" ("Big_int.to_string t" 42))
    ("-6 * 7" ("Big_int.to_string t" -42))
    ("6 * -7" ("Big_int.to_string t" -42))
    ("-6 * -7" ("Big_int.to_string t" 42))
    ("0 * 5" ("Big_int.to_string t" 0))
    ("limb boundary" ("Big_int.to_string t" 1073741824))
    ("big * big"
     ("Big_int.to_string t"
      121932631137021795226185032733622923332237463801111263526900))
    ("big * small" ("Big_int.to_string t" -123456789012345678901234567890000000))
    |}]
;;

let%expect_test "division and remainder" =
  let show name q r =
    print_s
      [%message
        name ~q:(Big_int.to_string q : string) ~r:(Big_int.to_string r : string)]
  in
  let dv name a b =
    let q, r = Big_int.div_rem (Big_int.of_string a) (Big_int.of_string b) in
    show name q r
  in
  dv "7 / 2" "7" "2";
  dv "-7 / 2" "-7" "2";
  dv "7 / -2" "7" "-2";
  dv "-7 / -2" "-7" "-2";
  dv "10 / 5" "10" "5";
  dv "-10 / 5" "-10" "5";
  dv "10 / -5" "10" "-5";
  dv "-10 / -5" "-10" "-5";
  dv "0 / 5" "0" "5";
  dv "big / small" "123456789012345678901234567890" "123456789";
  dv
    "big / big"
    "123456789012345678901234567890123456789"
    "98765432109876543210";
  (* Verify q * b + r = a. *)
  let a = Big_int.of_string "123456789012345678901234567890123456789" in
  let b = Big_int.of_string "98765432109876543210" in
  let q, r = Big_int.div_rem a b in
  print_s
    [%message "q*b+r = a" ~ok:(Big_int.equal Big_int.((q * b) + r) a : bool)];
  print_s
    [%message
      "division by zero"
        ~bomb:
          ((try
              ignore (Big_int.div_rem a Big_int.zero);
              "no raise"
            with
            | _ -> "<raised>")
           : string)];
  [%expect
    {|
    ("7 / 2" (q 3) (r 1))
    ("-7 / 2" (q -3) (r -1))
    ("7 / -2" (q -3) (r 1))
    ("-7 / -2" (q 3) (r -1))
    ("10 / 5" (q 2) (r 0))
    ("-10 / 5" (q -2) (r 0))
    ("10 / -5" (q -2) (r 0))
    ("-10 / -5" (q 2) (r 0))
    ("0 / 5" (q 0) (r 0))
    ("big / small" (q 1000000000100000000010) (r 0))
    ("big / big" (q 1249999988609375000) (r 15297067891529706789))
    ("q*b+r = a" (ok true))
    ("division by zero" (bomb <raised>))
    |}]
;;

let%expect_test "operators agree with div_rem" =
  let x = Big_int.of_string "-98765432109876543210" in
  let y = Big_int.of_int 123456789 in
  print_s
    [%message
      "operators"
        ~q:(Big_int.to_string Big_int.(x / y) : string)
        ~r:(Big_int.to_string Big_int.(x % y) : string)];
  [%expect {| (operators (q -800000007370) (r -8280)) |}]
;;

let%expect_test "gcd" =
  let show a b =
    print_s
      [%message
        (a : string)
          (b : string)
          ~gcd:
            (Big_int.to_string
               (Big_int.gcd (Big_int.of_string a) (Big_int.of_string b))
             : string)]
  in
  show "12" "18";
  show "18" "12";
  show "17" "5";
  show "0" "5";
  show "5" "0";
  show "0" "0";
  show "-12" "18";
  show "12" "-18";
  show "123456789012345678901234567890" "98765432109876543210";
  show "4611686018427387904" "1234567890123456789";
  [%expect
    {|
    ((a 12) (b 18) (gcd 6))
    ((a 18) (b 12) (gcd 6))
    ((a 17) (b 5) (gcd 1))
    ((a 0) (b 5) (gcd 5))
    ((a 5) (b 0) (gcd 5))
    ((a 0) (b 0) (gcd 0))
    ((a -12) (b 18) (gcd 6))
    ((a 12) (b -18) (gcd 6))
    ((a 123456789012345678901234567890) (b 98765432109876543210) (gcd 90))
    ((a 4611686018427387904) (b 1234567890123456789) (gcd 1))
    |}]
;;

let%expect_test "is_even" =
  let show s =
    print_s
      [%message
        (s : string) ~even:(Big_int.is_even (Big_int.of_string s) : bool)]
  in
  show "0";
  show "1";
  show "2";
  show "-1";
  show "-2";
  show "12345678901234567890";
  show "12345678901234567891";
  [%expect
    {|
    ((s 0) (even true))
    ((s 1) (even false))
    ((s 2) (even true))
    ((s -1) (even false))
    ((s -2) (even true))
    ((s 12345678901234567890) (even true))
    ((s 12345678901234567891) (even false))
    |}]
;;

let%expect_test "to_int boundaries" =
  let max_str = Int.to_string Int.max_value in
  let min_str = Int.to_string Int.min_value in
  let show name t = print_s [%message name (Big_int.to_int t : int option)] in
  show "of_int max" (Big_int.of_int Int.max_value);
  show "max_str" (Big_int.of_string max_str);
  show "above max" (Big_int.of_string (max_str ^ "0"));
  show "of_int min" (Big_int.of_int Int.min_value);
  show "min_str" (Big_int.of_string min_str);
  show "below min" (Big_int.of_string (min_str ^ "0"));
  show "zero" Big_int.zero;
  show "one" Big_int.one;
  show "minus_one" Big_int.minus_one;
  print_s
    [%message
      "round trips"
        ~max:
          (Big_int.equal
             (Big_int.of_int Int.max_value)
             (Big_int.of_string max_str)
           : bool)
        ~min:
          (Big_int.equal
             (Big_int.of_int Int.min_value)
             (Big_int.of_string min_str)
           : bool)];
  [%expect
    {|
    ("of_int max" ("Big_int.to_int t" (4611686018427387903)))
    (max_str ("Big_int.to_int t" (4611686018427387903)))
    ("above max" ("Big_int.to_int t" ()))
    ("of_int min" ("Big_int.to_int t" (-4611686018427387904)))
    (min_str ("Big_int.to_int t" (-4611686018427387904)))
    ("below min" ("Big_int.to_int t" ()))
    (zero ("Big_int.to_int t" (0)))
    (one ("Big_int.to_int t" (1)))
    (minus_one ("Big_int.to_int t" (-1)))
    ("round trips" (max true) (min true))
    |}]
;;

let%expect_test "compare min max" =
  let show name a b =
    print_s
      [%message
        name
          ~a:(Big_int.to_string a : string)
          ~b:(Big_int.to_string b : string)
          ~cmp:(Big_int.compare a b : int)
          ~min:(Big_int.to_string (Big_int.min a b) : string)
          ~max:(Big_int.to_string (Big_int.max a b) : string)]
  in
  show "-5 vs 3" (Big_int.of_int (-5)) (Big_int.of_int 3);
  show "3 vs 3" (Big_int.of_int 3) (Big_int.of_int 3);
  show "-5 vs -3" (Big_int.of_int (-5)) (Big_int.of_int (-3));
  show "0 vs -5" Big_int.zero (Big_int.of_int (-5));
  show
    "big vs small"
    (Big_int.of_string "123456789012345678901234567890")
    (Big_int.of_string "-123456789012345678901234567890");
  [%expect
    {|
    ("-5 vs 3" (a -5) (b 3) (cmp -1) (min -5) (max 3))
    ("3 vs 3" (a 3) (b 3) (cmp 0) (min 3) (max 3))
    ("-5 vs -3" (a -5) (b -3) (cmp -1) (min -5) (max -3))
    ("0 vs -5" (a 0) (b -5) (cmp 1) (min -5) (max 0))
    ("big vs small" (a 123456789012345678901234567890)
     (b -123456789012345678901234567890) (cmp 1)
     (min -123456789012345678901234567890) (max 123456789012345678901234567890))
    |}]
;;

let%expect_test "sexp round trip" =
  let t = Big_int.of_string "-123456789012345678901234567890" in
  let sexp = [%sexp (t : Big_int.t)] in
  print_s
    [%message
      (sexp : Sexp.t)
        ~back:(Big_int.to_string ([%of_sexp: Big_int.t] sexp) : string)];
  [%expect
    {|
    ((sexp -123456789012345678901234567890)
     (back -123456789012345678901234567890))
    |}]
;;

let%expect_test "cross-check against Int on fixed small cases" =
  let pairs =
    [ 0, 1
    ; 0, -1
    ; 1, 1
    ; 2, 3
    ; -2, 3
    ; 2, -3
    ; -2, -3
    ; 7, 2
    ; -7, 2
    ; 7, -2
    ; -7, -2
    ; 100, 10
    ; -100, 10
    ; 100, -10
    ; -100, -10
    ; 123456789, 987654321
    ; -123456789, 987654321
    ; 123456789, -987654321
    ; 1 lsl 30, 1 lsl 30
    ; (1 lsl 30) - 1, (1 lsl 30) - 1
    ; (1 lsl 30) + 1, (1 lsl 30) - 1
    ; Int.max_value / 3, 3
    ; Int.min_value / 3, -3
    ]
  in
  let failures =
    List.concat_map pairs ~f:(fun (a, b) ->
      let ba = Big_int.of_int a in
      let bb = Big_int.of_int b in
      let check name (got : Big_int.t) expected =
        if Option.equal Int.equal (Big_int.to_int got) (Some expected)
        then []
        else
          [ Printf.sprintf
              "%d %s %d: got %s, expected %d"
              a
              name
              b
              (Big_int.to_string got)
              expected
          ]
      in
      let div_failures =
        if b = 0
        then []
        else (
          let q, r = Big_int.div_rem ba bb in
          check "/" q (Int.( / ) a b) @ check "rem" r (Int.rem a b))
      in
      check "+" Big_int.(ba + bb) (Int.( + ) a b)
      @ check "-" Big_int.(ba - bb) (Int.( - ) a b)
      @ check "*" Big_int.(ba * bb) (Int.( * ) a b)
      @ div_failures)
  in
  print_s
    [%message
      "cross-check" ~count:(List.length failures : int) (failures : string list)];
  [%expect {| (cross-check (count 0) (failures ())) |}]
;;
