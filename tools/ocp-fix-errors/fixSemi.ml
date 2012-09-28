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

(*
  - Adds a ; at the end of the expression, when it seems applied to another argument. (Error)
*)

open ErrorLocation

let fix loc =
  let abs_end_pos = loc.loc_end_pos in
  [loc.loc_file, abs_end_pos+1, abs_end_pos+1, ";"],
  "Semi-colon inserted, file saved"

let fix_line loc =
  let pos = loc.loc_end_pos in
  let file = loc.loc_file in
  let s = file.file_content in
  let len = String.length s in
  let rec iter pos0 pos =
    if pos < len then
      match s.[pos] with
          '\r' | '\n' -> pos0
        | ' ' | '\t' -> iter pos0 (pos+1)
        | _ -> iter (pos+1) (pos+1)
    else
      len
  in
  let abs_end_pos = iter pos pos in
  [loc.loc_file, abs_end_pos, abs_end_pos, ";"],
  "Semi-colon inserted, file saved"
