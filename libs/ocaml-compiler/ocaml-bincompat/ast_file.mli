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

(** This is an entry point for reading of OCaml ASTs for the supported
    versions ("3.12" and "current"), as well as Camlp4 ASTs for
    version 3.12 only. *)

(* exception Outdated_version *)

(* can raise Outdated_version if the version is not recognized *)
val input_interface : string -> in_channel -> string * Parsetree.signature
val input_implementation : string -> in_channel -> string * Parsetree.structure

(*
val write : ?version: Bincompat.version -> string -> t -> unit
*)
