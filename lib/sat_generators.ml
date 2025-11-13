open! Core
open! Import

(** QuickCheck generators for SAT problem instances of various shapes.
    These generators are designed to test SAT solvers effectively. *)

(** Generate a random k-SAT instance with uniform random clause generation.

    This is the simplest generator - randomly select k variables for each clause,
    randomly negate each literal with probability 0.5.

    @param num_vars Number of variables in the problem
    @param num_clauses Number of clauses to generate
    @param k Clause length (all clauses have exactly k literals)
    @return Array of clauses, where each clause is an int array *)
let uniform_random_k_sat ~num_vars ~num_clauses ~k =
  Quickcheck.Generator.create (fun ~size:_ ~random ->
    Array.init num_clauses ~f:(fun _ ->
      (* Generate k distinct variables *)
      let vars = ref [] in
      while List.length !vars < k do
        let var = 1 + Splittable_random.int random ~lo:0 ~hi:(num_vars - 1) in
        if not (List.mem !vars var ~equal:Int.equal)
        then vars := var :: !vars
      done;
      (* Randomly negate literals *)
      Array.of_list_map !vars ~f:(fun var ->
        if Splittable_random.bool random then var else -var)))
;;

(** Generate a k-SAT instance with a fixed clause-to-variable ratio.

    The phase transition for 3-SAT occurs around ratio 4.26. Instances near
    this ratio are typically the hardest to solve.

    @param num_vars Number of variables in the problem
    @param ratio Clause-to-variable ratio (e.g., 4.3 for near-critical 3-SAT)
    @param k Clause length
    @return Array of clauses *)
let fixed_ratio_k_sat ~num_vars ~ratio ~k =
  let num_clauses = Float.to_int (ratio *. Float.of_int num_vars) in
  uniform_random_k_sat ~num_vars ~num_clauses ~k
;;

(** Generate a SAT instance with a planted solution.

    This starts with a satisfying assignment and generates clauses that are
    satisfied by it. Guarantees the instance is satisfiable. The difficulty
    can be controlled by the number of satisfied literals per clause.

    @param num_vars Number of variables
    @param num_clauses Number of clauses to generate
    @param k Clause length
    @param min_satisfied Minimum number of literals in each clause satisfied by the planted solution
    @return Tuple of (clauses, planted solution as bool array) *)
let planted_solution ~num_vars ~num_clauses ~k ~min_satisfied =
  Quickcheck.Generator.create (fun ~size:_ ~random ->
    (* Generate a random satisfying assignment *)
    let solution =
      Array.init num_vars ~f:(fun _ -> Splittable_random.bool random)
    in
    let clauses =
      Array.init num_clauses ~f:(fun _ ->
        (* Generate k distinct variables *)
        let vars = ref [] in
        while List.length !vars < k do
          let var = Splittable_random.int random ~lo:0 ~hi:(num_vars - 1) in
          if not (List.mem !vars var ~equal:Int.equal)
          then vars := var :: !vars
        done;
        (* Ensure at least min_satisfied literals are satisfied *)
        let clause_vars = Array.of_list !vars in
        let state = Random.State.make [| Splittable_random.int random ~lo:0 ~hi:Int.max_value |] in
        Array.permute ~random_state:state clause_vars;
        Array.mapi clause_vars ~f:(fun i var ->
          let lit_value =
            if i < min_satisfied
            then (* Force this literal to be satisfied *)
              solution.(var)
            else (* Randomly choose polarity *)
              Splittable_random.bool random
          in
          if lit_value then var + 1 else -(var + 1)))
    in
    clauses, solution)
;;

(** Generate a scale-free SAT instance where variable frequencies follow a power law.

    This models real-world SAT instances where some variables appear very frequently
    and most appear rarely. More realistic than uniform random generation.

    @param num_vars Number of variables
    @param num_clauses Number of clauses
    @param k Clause length
    @param alpha Power law exponent (typically 2.0-3.0; lower = more skewed)
    @return Array of clauses *)
let scale_free_k_sat ~num_vars ~num_clauses ~k ~alpha =
  Quickcheck.Generator.create (fun ~size:_ ~random ->
    (* Build a power-law distribution for variable selection *)
    let weights =
      Array.init num_vars ~f:(fun i ->
        1.0 /. (Float.of_int (i + 1) ** alpha))
    in
    let total_weight = Array.fold weights ~init:0.0 ~f:Float.( + ) in
    let normalized_weights =
      Array.map weights ~f:(fun w -> w /. total_weight)
    in

    (* Cumulative distribution for sampling *)
    let cumulative =
      Array.folding_map normalized_weights ~init:0.0 ~f:(fun acc w ->
        let new_acc = acc +. w in
        new_acc, new_acc)
    in

    let sample_var () =
      let r = Splittable_random.float random ~lo:0.0 ~hi:1.0 in
      match
        Array.findi cumulative ~f:(fun _ cumul -> Float.(cumul >= r))
      with
      | Some (i, _) -> i + 1
      | None -> num_vars
    in

    Array.init num_clauses ~f:(fun _ ->
      (* Generate k distinct variables using power-law distribution *)
      let vars = Hash_set.create (module Int) in
      while Hash_set.length vars < k do
        Hash_set.add vars (sample_var ())
      done;
      (* Randomly negate literals *)
      Hash_set.to_array vars
      |> Array.map ~f:(fun var ->
        if Splittable_random.bool random then var else -var)))
;;

(** Generate a random k-CNF instance (variable clause lengths between 1 and k).

    Unlike k-SAT where all clauses have exactly k literals, this allows
    clauses of varying lengths up to k.

    @param num_vars Number of variables
    @param num_clauses Number of clauses
    @param max_k Maximum clause length
    @return Array of clauses *)
let random_k_cnf ~num_vars ~num_clauses ~max_k =
  Quickcheck.Generator.create (fun ~size:_ ~random ->
    Array.init num_clauses ~f:(fun _ ->
      let clause_len = 1 + Splittable_random.int random ~lo:0 ~hi:(max_k - 1) in
      let vars = ref [] in
      while List.length !vars < clause_len do
        let var = 1 + Splittable_random.int random ~lo:0 ~hi:(num_vars - 1) in
        if not (List.mem !vars var ~equal:Int.equal)
        then vars := var :: !vars
      done;
      Array.of_list_map !vars ~f:(fun var ->
        if Splittable_random.bool random then var else -var)))
;;

(** Generate a SAT instance with forced backbone variables.

    Some variables are forced to have specific values in any solution.
    This creates harder instances with a constrained solution space.

    @param num_vars Number of variables
    @param num_clauses Number of clauses
    @param k Clause length
    @param backbone_size Number of variables in the backbone
    @return Tuple of (clauses, backbone as (var, value) list) *)
let forced_backbone ~num_vars ~num_clauses ~k ~backbone_size =
  Quickcheck.Generator.create (fun ~size:_ ~random ->
    (* Select backbone variables and their forced values *)
    let backbone_vars =
      List.init backbone_size ~f:(fun _ -> 1 + Splittable_random.int random ~lo:0 ~hi:(num_vars - 1))
      |> List.dedup_and_sort ~compare:Int.compare
    in
    let backbone =
      List.map backbone_vars ~f:(fun var -> var, Splittable_random.bool random)
    in

    (* Generate clauses, ensuring backbone constraints are satisfied *)
    let clauses =
      Array.init num_clauses ~f:(fun _ ->
        let vars = ref [] in
        while List.length !vars < k do
          let var = 1 + Splittable_random.int random ~lo:0 ~hi:(num_vars - 1) in
          if not (List.mem !vars var ~equal:Int.equal)
          then vars := var :: !vars
        done;

        Array.of_list_map !vars ~f:(fun var ->
          match List.Assoc.find backbone ~equal:Int.equal var with
          | Some value ->
            (* This variable is in the backbone - ensure clause is satisfied *)
            if value then var else -var
          | None ->
            (* Not a backbone variable - choose randomly *)
            if Splittable_random.bool random then var else -var))
    in

    (* Add unit clauses to force the backbone *)
    let backbone_clauses =
      Array.of_list_map backbone ~f:(fun (var, value) ->
        [| if value then var else -var |])
    in

    Array.append backbone_clauses clauses, backbone)
;;

(** Generate hard 3-SAT instances near the phase transition.

    This is a convenience function that generates 3-SAT instances with
    ratio 4.26, which is near the phase transition and typically produces
    the hardest instances.

    @param num_vars Number of variables
    @return Array of clauses *)
let hard_3_sat ~num_vars =
  fixed_ratio_k_sat ~num_vars ~ratio:4.26 ~k:3
;;

(** Generate an easy planted 3-SAT instance.

    Guaranteed satisfiable with at least 2 out of 3 literals satisfied
    in each clause. This makes it easier to find a solution.

    @param num_vars Number of variables
    @param num_clauses Number of clauses
    @return Tuple of (clauses, solution) *)
let easy_planted_3_sat ~num_vars ~num_clauses =
  planted_solution ~num_vars ~num_clauses ~k:3 ~min_satisfied:2
;;
