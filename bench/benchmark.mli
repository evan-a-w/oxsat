open! Core

(** Configuration for benchmarking *)
module Config : sig
  type t =
    { warmup_runs : int (** Number of warmup iterations before sampling *)
    ; sample_runs : int (** Number of samples to collect *)
    ; min_iterations : int (** Minimum iterations per sample *)
    ; max_iterations : int (** Maximum iterations per sample *)
    ; target_time_ns : int (** Target time in nanoseconds for each sample *)
    }

  (** Default configuration:
      - warmup_runs: 1
      - sample_runs: 100
      - min_iterations: 1
      - max_iterations: 1_000_000_000
      - target_time_ns: 100_000_000 (100ms)
  *)
  val default : t

  (** Create a custom configuration *)
  val create
    :  ?warmup_runs:int
    -> ?sample_runs:int
    -> ?min_iterations:int
    -> ?max_iterations:int
    -> ?target_time_ns:int
    -> unit
    -> t
end

(** Statistical results *)
module Stats : sig
  type t =
    { mean : float
    ; stddev : float
    ; p50 : float
    ; p75 : float
    ; p90 : float
    ; p95 : float
    ; p99 : float
    ; samples : int
    }
  [@@deriving sexp_of]

  (** Format stats in nanoseconds *)
  val to_string_ns : t -> string

  (** Format stats with human-readable units (ns/μs/ms/s) *)
  val to_string_readable : t -> string
end

(** Benchmark result *)
module Result : sig
  type t =
    { name : string
    ; stats : Stats.t
    }
  [@@deriving sexp_of]

  val to_string : t -> string
end

(** Run a single benchmark and return statistics *)
val run : ?config:Config.t -> name:string -> (unit -> 'a) -> Result.t

(** Run multiple benchmarks *)
val run_all : ?config:Config.t -> (string * (unit -> 'a)) list -> Result.t list

(** Run a single benchmark and print results *)
val run_and_print : ?config:Config.t -> name:string -> (unit -> 'a) -> Result.t

(** Run multiple benchmarks and print results *)
val run_all_and_print : ?config:Config.t -> (string * (unit -> 'a)) list -> Result.t list

(** Print benchmark results *)
val print_results : Result.t list -> unit
