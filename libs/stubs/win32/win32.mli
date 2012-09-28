type os_type = WINDOWS | CYGWIN | UNIX

val os_type : os_type


external waitpids : int -> int array -> int * Unix.process_status
  = "win32_waitpids_ml"

val waitpid : int -> int

val command : string array -> int
val simulate_exec : string array -> 'a
