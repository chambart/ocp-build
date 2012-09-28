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

type error =
    FileTooShort
  | BadMagic of string  (* what was read *)

exception Error of
    string  (* filename *)
  * error   (* the error *)

module RAW : sig

  type t = {
    header : string;
    sections : (string * string) list;
    magic : string;
  }
  val load : string -> t
  val save : string -> t -> unit

end


type t = {
  header : string;
  mutable primitives : string array;
  mutable data : Obj.t array;
  mutable nsymbols : int;
  mutable symbols : (Ident.t, int) Tbl.t;
  mutable debug : (int * Instruct.debug_event list) list;
  mutable code : string;
  mutable dll_path : string list;
  mutable dll_names : string list;
  mutable imports : (string * Digest.t) list;
  mutable runtime : string option;

  mutable other_sections : (string * string) list;
  magic : string;

  raw : RAW.t;
}

val load : string -> t
val save : string -> t -> unit

val string_of_file : t -> string
val string_of_error : error -> string

val is_bytecode_executable : string -> bool

