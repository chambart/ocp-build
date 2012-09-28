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
  - Add missing labels when creating an array (Error)
*)

open ErrorLocation
include Debug.Tag(struct let tag = "fixRecord" end)

let fix loc error_line next_lines =
  let indented_lines = FixUtils.find_indented [] next_lines in
  let (_, error_line) = OcpString.cut_at error_line ':' in
  let (_, labels) = OcpString.cut_at error_line ':' in
  let labels = String.concat " " (labels :: indented_lines) in
  debug "labels: [%s]\n%!" labels;
  let labels = OcpString.split_simplify labels ' ' in

  let abs_begin_pos = loc.loc_begin_pos in
  let abs_end_pos = loc.loc_end_pos in
  debug "Filename: %s\n%!"
    (File.to_string loc.loc_file.file_file);

  let expr = String.sub loc.loc_file.file_content
    abs_begin_pos (abs_end_pos - abs_begin_pos) in

  let rec find_indent expr begin_pos pos =
    match expr.[pos] with
        ' ' | '\t' | '{' | '(' -> find_indent expr (begin_pos+1) (pos+1)
      | _ -> begin_pos
  in
  let indent = find_indent expr (loc.loc_begin_pos - loc.loc_bol) 0 in

  let need_semicolon = ref true in
  let rec find_end expr brace abs_end_pos pos =
    match expr.[pos] with
        ' ' | '\t' | '\n' | '\r' -> find_end expr brace (abs_end_pos-1) (pos-1)
      | ';' -> need_semicolon := false; abs_end_pos
      | '}' when not brace ->
        find_end expr true (abs_end_pos-1) (pos-1)
      | _ when not brace ->
        find_end expr brace (abs_end_pos-1) (pos-1)
      | _ -> abs_end_pos
  in
  let abs_end_pos = find_end expr false abs_end_pos (String.length expr - 1) in

  debug "Expression: <%s>\n%!" expr;

  let indent = String.make indent ' ' in
  let first, labels =
    match labels with
      | first :: labels -> first, labels
      | _ -> assert false
  in
  let b =Buffer.create 100 in
  if !need_semicolon then Printf.bprintf b ";";
  Printf.bprintf b "\n";
  Printf.bprintf b "%s%s = assert false" indent first;
  List.iter (Printf.bprintf b ";\n%s%s = assert false" indent) labels;
  [loc.loc_file, abs_end_pos, abs_end_pos, Buffer.contents b
  ],
  "Inserted stubs for missing labels"
