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

(* Tifn: we don't use this currently, because Edit takes care of
   multiple replacement. This could be an alternative to alignment
   with Diff, but only if we know that no other modifications happen
   to the file. *)

type edit_op =
    Insertion of (*pos: *) int * (*len: *) int
  | Deletion of (*pos: *) int * (*len: *) int

type file = {
  file_file : File.t;
  file_content : string;
  mutable file_ops : edit_op list;
}

type loc = {
  loc_file : file;
  loc_bol : int;
  loc_line_pos : int;
  loc_begin_pos : int;
  loc_end_pos : int;
}

val parse_location : string -> string -> loc
val find_file : string -> file

val final_pos : file -> int -> int
val add_edition : file -> edit_op -> unit

val reset : unit -> unit
