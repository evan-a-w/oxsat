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
      - target_time_ns: 100_000_000 (100ms) *)
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
    ; alloc_bytes : float (** Mean bytes allocated per iteration *)
    ; major_collections : float (** Mean major GC collections per iteration *)
    }
  [@@deriving sexp]

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
  [@@deriving sexp]

  val to_string : t -> string
end

(** Run a single benchmark and return statistics *)
val run : ?config:Config.t -> name:string -> (unit -> 'a) -> Result.t

(** Whether [name] passes the [only] filter (empty [only] matches everything). *)
val matches_only : only:string list -> string -> bool

(** Run multiple benchmarks. If [only] is non-empty, only benchmarks whose name
    contains one of the given substrings are run. *)
val run_all
  :  ?config:Config.t
  -> ?only:string list
  -> (string * (unit -> 'a)) list
  -> Result.t list

(** Run a single benchmark and print results *)
val run_and_print : ?config:Config.t -> name:string -> (unit -> 'a) -> Result.t

(** Run multiple benchmarks and print results *)
val run_all_and_print
  :  ?config:Config.t
  -> ?only:string list
  -> (string * (unit -> 'a)) list
  -> Result.t list

(** Print benchmark results *)
val print_results : Result.t list -> unit

(** Save/load results as sexp, for later comparison with [compare_results]. *)
val save_results : filename:string -> Result.t list -> unit

val load_results : filename:string -> Result.t list

(** Print a table comparing two sets of results, sorted by largest time
    regression first. Ratios are [after / before], so > 1x is a regression. *)
val compare_results : before:Result.t list -> after:Result.t list -> unit
