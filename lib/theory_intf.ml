module type S = sig
  type t

  val create : unit -> t

  (** Called immediately when a literal is pushed onto the trail. The theory
      should update its internal state incrementally. [decision_level] is the
      current decision level at the time of the push, so the theory can tag
      each state change and retract it precisely on [pop]. *)
  val assert_literal : t -> decision_level:int -> int -> unit

  (** Called at propagation quiescence. Returns:
      - [`Consistent] — theory is satisfied; SAT engine may proceed
      - [`Conflict of int array] — literals jointly T-inconsistent; SAT engine
        learns this as a clause and backtracks
      - [`Propagate of (int * int array) list] — list of
        [(literal, explanation_clause)] pairs the theory has deduced must hold *)
  val check_consistent
    :  t
    -> [ `Consistent
       | `Conflict of int array
       | `Propagate of (int * int array) list
       ]

  (** Called once after the trail is unwound to [to_decision_level], allowing
      the theory to retract any state accumulated above that level. *)
  val pop : t -> to_decision_level:int -> unit

  (** Called when a new Boolean variable is introduced, so theories that map
      variables to T-atoms can register the mapping. *)
  val on_new_var : t -> var:int -> unit
end
