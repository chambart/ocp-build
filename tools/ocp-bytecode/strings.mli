(* Copyright: OCamlPro *)
(* Author: Fabrice LE FESSANT (OCamlPro) *)

type t

val create : unit -> t
val add_immutable_string : t -> string -> unit
val add_char : t -> char -> unit
val printf : t -> ('a, unit, string, unit) format4 -> 'a

val length : t -> int
val contents : t -> string
