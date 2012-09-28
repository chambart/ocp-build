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

open SimpleConfig

  type project_options = {
    mutable option_ncores : int;
    mutable option_autoscan : bool;
    mutable option_verbosity : int;
    mutable option_usestdlib : bool;
    mutable option_digest : bool;
    mutable option_bytecode : bool;
    mutable option_native : bool;

    mutable option_ocamlbin : string;
    mutable option_ocamllib : string;

    mutable option_ocamlc : string list;
    mutable option_ocamlopt : string list;
    mutable option_ocamldep : string list;
    mutable option_ocamllex : string list;
    mutable option_ocamlyacc : string list;
  }

module GlobalOptions : sig

  val verbosity_option : int config_option

end


val load_global : unit -> unit
val maybe_save_global : unit -> unit
val save_global : unit -> unit

val load_local : File.t -> SimpleConfig.config_file * project_options
val save_local : SimpleConfig.config_file -> unit
val maybe_save_local : SimpleConfig.config_file -> unit


val arg_list : unit -> (string * Arg.spec * string) list
val shortcut_arg :
  string -> string ->
(string * Arg.spec * string) list ->
  (string * Arg.spec * string)
