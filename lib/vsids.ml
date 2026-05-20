open! Core
open! Import

module Heap = struct
  type t =
    { literals : int Vec.Value.t
    ; scores : F64.Vec.t
    ; mutable length : int
    }

  let create () =
    { literals = Vec.Value.create (); scores = F64.Vec.create (); length = 0 }
  ;;

  let clear t =
    Vec.Value.clear t.literals;
    F64.Vec.clear t.scores;
    t.length <- 0
  ;;

  let[@inline] compare_at t i j =
    match F64.compare (F64.Vec.get t.scores i) (F64.Vec.get t.scores j) with
    | 0 -> Int.compare (Vec.Value.get t.literals i) (Vec.Value.get t.literals j)
    | o -> o
  ;;

  let[@inline] better_at t i j = compare_at t i j > 0

  let[@inline] swap t i j =
    let lit = Vec.Value.get t.literals i in
    let score = F64.Vec.get t.scores i in
    Vec.Value.set t.literals i (Vec.Value.get t.literals j);
    F64.Vec.set t.scores i (F64.Vec.get t.scores j);
    Vec.Value.set t.literals j lit;
    F64.Vec.set t.scores j score
  ;;

  let rec bubble_up t idx =
    if idx > 0
    then (
      let parent = (idx - 1) / 2 in
      if better_at t idx parent
      then (
        swap t idx parent;
        bubble_up t parent))
  ;;

  let rec bubble_down t idx =
    let left = (idx * 2) + 1 in
    if left < t.length
    then (
      let right = left + 1 in
      let best =
        if right < t.length && better_at t right left then right else left
      in
      if better_at t best idx
      then (
        swap t idx best;
        bubble_down t best))
  ;;

  let push t literal score =
    Vec.Value.push t.literals literal;
    F64.Vec.push t.scores score;
    t.length <- t.length + 1;
    bubble_up t (t.length - 1)
  ;;

  let peek_literal t =
    if t.length = 0 then None else Some (Vec.Value.get t.literals 0)
  ;;

  let peek_score t = F64.Vec.get t.scores 0

  let pop_exn t =
    if t.length = 0
    then raise (Invalid_argument "Empty heap")
    else (
      let last_literal = Vec.Value.pop_exn t.literals in
      let last_score = F64.Vec.pop_exn t.scores in
      t.length <- t.length - 1;
      if t.length > 0
      then (
        Vec.Value.set t.literals 0 last_literal;
        F64.Vec.set t.scores 0 last_score;
        bubble_down t 0);
      ())
  ;;
end

type t =
  { heap : Heap.t
  ; score_by_literal : F64.Option.Vec.t Tf_pair.t
  ; in_pool : Bitset.t
  ; mutable adjusting_score : Adjusting_score.t
  }

let create () =
  { heap = Heap.create ()
  ; score_by_literal =
      Tf_pair.create (fun (_ : bool) -> F64.Option.Vec.create ())
  ; in_pool = Bitset.create ()
  ; adjusting_score = Adjusting_score.default ()
  }
;;

let on_new_var t ~var =
  Tf_pair.iteri t.score_by_literal ~f:(fun ~key:value ~data:score_by_var ->
    F64.Option.Vec.fill_to_length
      score_by_var
      ~length:(var + 1)
      ~f:(fun (_ : int) -> F64.Option.none ());
    match%optional_u (F64.Option.Vec.get score_by_var var : F64.Option.t) with
    | Some _ -> ()
    | None ->
      let score = Adjusting_score.unit t.adjusting_score in
      F64.Option.Vec.set score_by_var var (F64.Option.some score);
      Bitset.set t.in_pool var;
      Heap.push t.heap (Literal.to_int (Literal.create ~var ~value)) score)
;;

let rebuild_heap t =
  Heap.clear t.heap;
  Tf_pair.iteri t.score_by_literal ~f:(fun ~key:_value ~data:score_by_var ->
    F64.Option.Vec.iteri score_by_var ~f:(fun i score ->
      match%optional_u.F64.Option score with
      | None -> ()
      | Some score when Bitset.get t.in_pool i ->
        Heap.push
          t.heap
          (Literal.create ~var:i ~value:_value |> Literal.to_int)
          score
      | Some _ -> ()))
;;

let rescale t =
  Tf_pair.iteri t.score_by_literal ~f:(fun ~key:_value ~data:score_by_var ->
    F64.Option.Vec.iteri score_by_var ~f:(fun i score ->
      match%optional_u.F64.Option score with
      | None -> ()
      | Some score ->
        let new_score = F64.O.(score / t.adjusting_score.#rescale) in
        F64.Option.Vec.set score_by_var i (F64.Option.some new_score)));
  rebuild_heap t;
  t.adjusting_score <- Adjusting_score.rescale t.adjusting_score
;;

let add_activity t ~literal =
  let vec = Tf_pair.get t.score_by_literal (Literal.value literal) in
  match%optional_u
    (F64.Option.Vec.get vec (Literal.var literal) : F64.Option.t)
  with
  | None ->
    F64.Option.Vec.set
      vec
      (Literal.var literal)
      (Adjusting_score.unit t.adjusting_score |> F64.Option.some)
  | Some score ->
    let new_score = F64.O.(score + Adjusting_score.unit t.adjusting_score) in
    F64.Option.Vec.set vec (Literal.var literal) (F64.Option.some new_score);
    if Bitset.get t.in_pool (Literal.var literal)
    then Heap.push t.heap (Literal.to_int literal) new_score;
    if F64.O.(new_score > t.adjusting_score.#rescale) then rescale t
;;

let decay t = t.adjusting_score <- Adjusting_score.decay t.adjusting_score
let remove_from_pool t ~var = Bitset.clear t.in_pool var

let add_to_pool t ~var =
  if not (Bitset.get t.in_pool var)
  then (
    Bitset.set t.in_pool var;
    let add value =
      Heap.push
        t.heap
        (Literal.create ~var ~value |> Literal.to_int)
        (F64.Option.Vec.get (Tf_pair.get t.score_by_literal value) var
         |> F64.Option.value_exn)
    in
    add true;
    add false)
;;

let choose_literal t =
  let rec go () =
    match Heap.peek_literal t.heap with
    | None -> Literal.Option.none ()
    | Some literal_int ->
      let score = Heap.peek_score t.heap in
      let literal = Literal.of_int literal_int in
      let var = Literal.var literal in
      if not (Bitset.get t.in_pool var)
      then (
        ignore (Heap.pop_exn t.heap);
        go ())
      else (
        let current_score =
          F64.Option.Vec.get
            (Tf_pair.get t.score_by_literal (Literal.value literal))
            var
          |> F64.Option.value_exn
        in
        if F64.equal current_score score
        then Literal.Option.some literal
        else (
          ignore (Heap.pop_exn t.heap);
          go ()))
  in
  go ()
;;
