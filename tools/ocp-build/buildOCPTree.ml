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

open OcpLang
(* open BuildEngineTypes *)

type camlpN =
  | Camlp4
  | Camlp5

type package_type =
    ProjectProgram
(*  | ProjectToplevel *)
  | ProjectLibrary
  | ProjectObjects

and condition =
  | IsEqualStringList of string * string list
  | IsTrue of string
  | NotCondition of condition
  | AndConditions of condition * condition
  | OrConditions of condition * condition

type statement =
    StmtOption of set_option
  | StmtBlock of statement list
  | StmtDefineConfig of string * set_option list
  | StmtDefinePackage of package_type * string * statement list
  | StmtFilesSet of (string * set_option list) list
  | StmtFilesAppend of (string * set_option list) list
  | StmtRequiresSet of string list
  | StmtRequiresAppend of string list
  | StmtIfThenElse of condition * statement list * statement list option
  | StmtSyntax of string option * camlpN * string list

and set_option =
    OptionListSet of string * string list
  | OptionListAppend of string * string list
  | OptionListRemove of string * string list
  | OptionBoolSet of string * bool
  | OptionConfigSet of string
  | OptionIfThenElse of condition * set_option list * set_option list option
  | OptionBlock of set_option list
(*  | OptionConfigAppend of string *)

let modname_of_fullname fullname =
  let modname = Filename.chop_extension (Filename.basename fullname) in
  modname.[0] <- Char.uppercase modname.[0];
  modname
