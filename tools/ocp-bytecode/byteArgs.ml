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

(* "Expunge" a toplevel by removing compiler modules from the global List.map.
   Usage: expunge <source file> <dest file> <names of modules to keep> *)

open Sys
open Misc

(*
let target_arg = ref "a.out"
let anon_args = ref ([] : string list)

type action =
    Dump
  | Expunge of string
  | KeepPrimitives of string
  | StripSections of string
  | ListSections of string
  | Disass
  | PrintPrims of string
  | PrintUsedPrims of string
  | SetPrimitiveTable of string
  | PrintLoadedPrims
(*  | DumpSection of string *)

let main_action = ref (None : (string * action) option)

let set_main_action name action =
  match !main_action with
      None -> main_action := Some (name, action)
    | Some (old_name, _) ->
      Printf.fprintf stderr "You cannot specify both actions %s and %s\n%!"
	old_name name;
      exit 2
*)

let current_file = ref (None : (string * ByteFile.t) option)
let current_action = ref (None : ( (string list ref) * (string list -> unit) ) option)
let must_save = ref false
