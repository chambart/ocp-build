(* var OCP_DEBUG_MODULES is a set of space separated module names *)

val add_submodules : string -> string list -> unit
val increase_verbosity : string -> int -> unit
val increase_verbosities : string list -> int -> unit

val verbose : string list -> string -> (int -> bool)
