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

open OcpSystem
open OcpLang
include Debug.Tag(struct let tag = "fixMain" end)

module EmacsLineParser : sig
  val parse_line : string -> string StringMap.t
  val extract_directory : string -> string

end = struct

  open Genlex

  let lexer = Genlex.make_lexer [ "-*-"; ":"; "-"; ";" ]

  let rec parse_tokens tokens =
    match tokens with
        Kwd "-*-" :: tail ->
          parse_options StringMap.empty [] tail
      | _ -> raise Parsing.Parse_error

  and parse_options map idents tokens =
    match tokens with
        Ident name :: Kwd "-" :: tail ->
          parse_options map (name :: idents) tail
      | Ident name :: Kwd ":" :: tail ->
        parse_value map (String.concat "-" (List.rev (name :: idents))) tail
      | Kwd "-*-" :: _ ->
        map
      | _ -> raise Parsing.Parse_error

  and parse_value map ident tokens =
    match tokens with
        String s :: tail ->
          let map = StringMap.add ident s map in
          parse_next map tail
      | Ident s :: tail -> (* TODO: use types to distinguish between strings and idents ? *)
        let map = StringMap.add ident s map in
        parse_next map tail
      | _ -> raise Parsing.Parse_error

  and parse_next map tokens =
    match tokens with
        Kwd ";" :: tail -> parse_options map [] tail
      | Kwd "-*-" :: tail -> map
      | _ -> raise Parsing.Parse_error

  let parse_line line =
    let tokens = Genlex.tokens_of_string lexer line in
    List.iter (fun token -> debugln "tok[%s]" (string_of_token token)) tokens;
    parse_tokens tokens

  let extract_directory s =
(* TODO: be more clever *)
    for i = 0 to String.length s - 1 do
      match s.[i] with
          '\'' -> s.[i] <- '"'
        | '`' -> s.[i] <- '"'
        | _ -> ()
    done;
    let tokens = Genlex.tokens_of_string lexer s in
    List.iter (fun token -> debugln "move:tok[%s]" (string_of_token token)) tokens;
    match tokens with
        [ Ident _; Ident "directory"; String s ] -> s
      | _ -> raise Parsing.Parse_error

end

let directory dir_stack =
  match dir_stack with
      [] -> assert false
    | directory :: _ -> directory

let rec skip_lines linenum line_pos dir_stack lines =
  match lines with
    | [] -> failwith "No error found"
    | line :: tail ->
      let (_, after) = String.cut_at line ':' in
      if OcpString.starts_with after " Entering directory" then
        let directory = EmacsLineParser.extract_directory after in
        skip_lines (linenum+1) line_pos (directory :: dir_stack) tail
      else
        if OcpString.starts_with after " Leaving directory" then
          let directory = EmacsLineParser.extract_directory after in
          match dir_stack with
              [] -> assert false
            | dir :: _ when dir <> directory -> assert false
            | _ :: dir_stack ->
              skip_lines (linenum+1) line_pos dir_stack tail
        else begin
          if linenum >= line_pos then
            try
              dir_stack,
              ErrorLocation.parse_location (directory dir_stack) line,
              tail
            with _ -> skip_lines (linenum+1) line_pos dir_stack  tail
          else
            skip_lines (linenum+1) line_pos dir_stack  tail
        end

let check_lines dir_stack loc lines =
  match lines with
      "Warning 8: this pattern-matching is not exhaustive." ::
        "Here is an example of a value that is not matched:" ::
        pattern_lines ->

          debugln "Found a non-exhaustive pattern matching";
          FixPattern.fix loc pattern_lines

    | error_line :: next_lines
        when
          OcpString.starts_with error_line
            "Error: Some record field labels are undefined:"
          ->
        debugln "Found an incomplete record";
              FixRecord.fix loc error_line next_lines

    | error_line :: next_lines
        when
          OcpString.starts_with error_line "Error: The implementation"   ->
        debugln "Found a non-matching interface";
              FixInterface.fix loc error_line next_lines

    | error_line :: next_lines when
        OcpString.starts_with error_line
          "Warning 26: unused variable"
        ||
          OcpString.starts_with error_line
          "Warning 27: unused variable"
        ->
        debugln "Found unused variable";
            FixUnusedVariables.fix loc (directory dir_stack) next_lines

    | error_line :: next_lines when
        OcpString.starts_with error_line
          "Warning 12: this sub-pattern is unused"
        ->
        debugln "Found unused pattern";
            FixUnusedPatterns.fix loc (directory dir_stack) next_lines

    | error_line :: next_lines when
        OcpString.starts_with error_line
          "Error: This expression has type" ->
        debugln "Found un-expected type";
              FixExpect.fix loc error_line next_lines, ""

    |
        "Error: This expression is not a function; it cannot be applied" :: _ ->
        debugln "Found a missing semi-colon";
            FixSemi.fix loc

    | "Error: This function is applied to too many arguments;" ::
        "maybe you forgot a `;'" :: _ ->
        debugln "Found a missing semi-colon (two many args)";
            FixSemi.fix_line loc

    | _ ->
        failwith "cannot fix"

let homedir = try Sys.getenv "HOME" with Not_found -> "/"

let expanse_directory name =
  if name = "" then "." else
    if name.[0] = '~' then
      Filename.concat homedir (String.sub name 1 (String.length name - 1))
    else name

let fix_next_error lines line_pos =
  match lines with
      [] -> failwith "Empty compilation buffer"
    | first_line :: other_lines ->
        let map = EmacsLineParser.parse_line first_line in
        assert (StringMap.find "mode" map = "compilation");
        let directory = expanse_directory (StringMap.find "default-directory" map) in
        let dir_stack, loc, lines = skip_lines 1 line_pos [directory] other_lines in
        debugln "found loc, next line: %s" (List.hd lines);
        let res = check_lines dir_stack loc lines in
        ErrorLocation.reset ();
        res

(*
let main () =
  Printf.eprintf "ocp-fix-errors started\n%!";
  let error_file = Sys.argv.(1) in
  let line_pos = int_of_string Sys.argv.(2) in
  let lines = FileLines.of_file error_file in
  fix_next_error lines line_pos

let _ =
  try
    main ()
  with e ->
    debug "Fatal error: Exception %s\n%!" (Printexc.to_string e);
    exit 2
*)
