(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

(* Copyright: OCamlPro *)
(* Author: Fabrice LE FESSANT (OCamlPro) *)

type t

val create : unit -> t
val add_immutable_string : t -> string -> unit
val add_char : t -> char -> unit
val printf : t -> ('a, unit, string, unit) format4 -> 'a

val length : t -> int
val contents : t -> string
