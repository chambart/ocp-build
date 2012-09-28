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

(** Reading camlp4 ( 3.12.* ) ASTs and mapping them to Parsetrees. *)

(** Magic numbers for camlp4 ASTs. These are the same since version
    3.10 ! *)

val camlp4_ast_impl_magic_number: string
val camlp4_ast_intf_magic_number: string

(** Reading functions, to call read the input after magic number. *)

val input_camlp4_ast_impl : in_channel -> string * Parsetree.structure
val input_camlp4_ast_intf : in_channel -> string * Parsetree.signature
