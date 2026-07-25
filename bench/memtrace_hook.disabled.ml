open! Core

let wrap f =
  (match Sys.getenv "OXSAT_MEMTRACE" with
   | None -> ()
   | Some _ ->
     eprintf
       "Warning: OXSAT_MEMTRACE is set, but memtrace is not installed; not \
        tracing\n");
  f ()
;;
