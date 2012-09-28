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

  open Subcommands.TYPES

  let arg_list = []
  let subcmd_spec = {
    subcmd_list = arg_list;
    subcmd_usage = [];
    subcmd_help = [];
  }
  let subcmd_main args = assert false
  let subcmd_init () = ()
