open! Core

let wrap f =
  match Sys.getenv "OXSAT_MEMTRACE" with
  | None -> f ()
  | Some filename ->
    let tracer =
      Memtrace.start_tracing
        ~context:(Some "feel_bench")
        ~sampling_rate:Memtrace.default_sampling_rate
        ~filename
    in
    Exn.protect ~f ~finally:(fun () -> Memtrace.stop_tracing tracer)
;;
