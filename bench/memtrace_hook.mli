(** Wraps [f], tracing allocations with memtrace to the file named by the
    OXSAT_MEMTRACE environment variable when it is set. When memtrace is not
    installed, tracing is disabled (with a warning if OXSAT_MEMTRACE is set). *)
val wrap : (unit -> 'a) -> 'a
