(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2012 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Ident

val name : t -> Name.t
val format : Format.formatter -> t -> unit
val unsafe_create_with_stamp : ?flags:int -> string -> int -> t
  (** create an ident with given flags and stamp *)
val parse : Name.t -> t
