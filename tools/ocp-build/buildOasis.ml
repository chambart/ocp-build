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

let find_indent line =
  let rec find_indent line i len =
    if i < len then
      match line.[i] with
          ' ' | '\t' -> find_indent line (i+1) len
        | '#' -> (i, true)
        | _ -> (i, false)
    else (i, false)
  in
  let len = String.length line in
  let (indent, comment) = find_indent line 0 len in
  (indent, comment, String.sub line indent (len - indent))

(* From oasis-0.2.1~alpha1%  less src/oasis/OASISRecDescParser.ml LGPL *)
let oasis_lexer = Genlex.make_lexer
  [
        (* Statement *)
    "+:"; "$:"; ":"; "if"; "{"; "}"; "else";
        (* Section *)
    "Flag"; "Library"; "Executable";
    "SourceRepository"; "Test";
    "Document";
        (* Expression *)
    "!"; "&&"; "||"; "("; ")"; "true"; "false"
  ]

type oasis_line =
    Line of string * oasis_line list ref

let read_oasis filename =
  let ic = open_in filename in
  let lines = ref [] in
  try
    let rec read_line stack ic =
      let line = input_line ic in
      let (indent, comment, line) = find_indent line in
      if comment then
        read_line stack ic
      else
        push_line stack ic indent line

    and push_line stack ic indent line =
      match stack with
          [] -> assert false
        | (current_indent, lines) :: previous_stack ->
            if indent = current_indent then begin
              lines := Line (line, ref []) :: !lines;
              read_line stack ic
            end else
              if indent < current_indent then
                push_line previous_stack ic indent line
              else (* indent > current_indent *)
                match !lines with
                    [] -> assert false
                  | Line (previous_line, new_lines) :: _ ->
                    new_lines := Line (line, ref []) :: !new_lines;
                    let stack = (indent, new_lines) :: stack in
                    read_line stack ic
    in
    read_line [(0, lines)] ic
  with End_of_file ->
    close_in ic;
    !lines

let print_oasis lines =
  let rec print indent lines =
    List.iter (fun line ->
      let Line (s, lines) = line in
      Printf.fprintf stderr "%s%s\n" indent s;
      print (indent ^ "___") !lines
    ) (List.rev lines)
  in
  print "" lines;
  Printf.fprintf stderr "%!"

let merge_line content lines =
  let lines = content :: (List.map (function Line (s, _) -> s) (List.rev !lines)) in
  String.concat " " lines

let parse_package kind name lines =
  Printf.fprintf stderr "parse_package %s\n%!" name;
  let open Genlex in
  List.iter (fun line ->
    let Line (s, lines) = line in
    let (header, content) = OcpString.cut_at s ':' in
    match String.lowercase header with
      | "modules" | "internalmodules" ->
          let line = merge_line content lines in
          Printf.fprintf stderr "[%s] FILES = %s\n%!" name line;
          ()
      | "path" ->
        let line = merge_line content lines in
        Printf.fprintf stderr "[%s] DIRNAME = %s\n%!" name line;
        ()
      | "builddepends" ->
        let line = merge_line content lines in
        Printf.fprintf stderr "[%s] REQUIRES = %s\n%!" name line;
        ()
      | _ ->
          Printf.fprintf stderr "[%s]Discarding line [%s]\n%!" name s
  ) (List.rev lines)


let parse_oasis lines =
  let open Genlex in
  let project_name = ref "" in
  List.iter (fun line ->
    let Line (s, lines) = line in
    try
      let tokens = OcpGenlex.tokens_of_string oasis_lexer s in
      match tokens with
          [ Ident "Name" ; Kwd ":" ; (String name | Ident name) ] ->
            project_name := name
        | [ Kwd "Library"; (String name | Ident name) ] ->
          let name = if name = !project_name then name else Printf.sprintf "%s.%s" !project_name name in
          parse_package "library" name !lines
        | [ Kwd "Executable"; (String name | Ident name) ] ->
          let name = if name = !project_name then name else Printf.sprintf "%s.%s" !project_name name in
          parse_package "program" (name ^ "-command") !lines
        | _ -> ()
    with _ ->
      Printf.fprintf stderr "Discarding line [%s]\n%!" s
  ) (List.rev lines)


let translate filename =
  let lines = read_oasis filename in
  print_oasis lines;
  parse_oasis lines

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    translate Sys.argv.(i)
  done


