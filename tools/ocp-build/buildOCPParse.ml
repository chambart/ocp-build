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

(* open SafeCaml *)
open Genlex
open BuildOCPTree
open BuildMisc
open BuildOCPVariable

open Ocamllexer
open BuildOCPParser

  let lexer = Ocamllexer.make_lexer
    [ "begin"; "end"; "true"; "false";
      "library"; "program"; "objects"; "config"; "include"; "type";
      "files"; "requires"; "file"; "use"; "pack";
      "if"; "then"; "else"; "syntax"; "camlp4"; "camlp5";
      "["; "]"; ";"; "("; ")"; "{"; "}"; "="; "+=";
      "not"; "&&"; "||"
    ]

let rec read_ocamlconf filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let token_of_token token_opt =
    match token_opt with
	None -> EOF
      | Some token ->
	match token with
	  | String s -> STRING s
	  | Float f -> FLOAT f
	  | Int i -> INT i
	  | Char c -> CHAR c
	  | Kwd ";" -> SEMI
	  | Kwd "[" -> LBRACKET
	  | Kwd "]" -> RBRACKET
	  | Kwd "(" -> LPAREN
	  | Kwd ")" -> RPAREN
          | Kwd "{" -> LBRACE
          | Kwd "}" -> RBRACE
	  | Kwd "begin" -> BEGIN
	  | Kwd "end" -> END
	  | Kwd "objects" -> OBJECTS
	  | Kwd "library" -> LIBRARY
	  | Kwd "config" -> CONFIG
	  | Kwd "use" -> USE
	  | Kwd "program" -> PROGRAM
	  | Kwd "type" -> TYPE
	  | Kwd "include" -> INCLUDE
	  | Kwd "=" -> EQUAL
	  | Kwd "+=" -> PLUSEQUAL
	  | Kwd "-=" -> MINUSEQUAL
	  | Kwd "true" -> TRUE
	  | Kwd "false" -> FALSE
	  | Kwd "files" -> FILES
	  | Kwd "file" -> FILE
	  | Kwd "requires" -> REQUIRES
	  | Kwd "pack" -> PACK
	  | Kwd "if" -> IF
	  | Kwd "then" -> THEN
	  | Kwd "else" -> ELSE
          | Kwd "not" -> NOT
          | Kwd "&&" -> COND_AND
          | Kwd "||" -> COND_OR
          | Kwd "syntax" -> SYNTAX
          | Kwd "camlp4" -> CAMLP4
          | Kwd "camlp5" -> CAMLP5
	  | Ident s -> IDENT s
	  | Kwd _ -> assert false
  in
  let dir = Filename.dirname filename in
  let trap_include lexbuf =
    match token_of_token (lexer lexbuf) with
    | INCLUDE ->
        let next_token = token_of_token (lexer lexbuf) in
        begin
          match next_token with
          | STRING inc_filename ->
            let inc_filename = if Filename.is_implicit inc_filename then
                Filename.concat dir inc_filename
              else
                inc_filename
            in
            if not (Sys.file_exists inc_filename) then begin
              Logger.warning "Warning: file %S does not exist.\n\t(included from %S)\n" inc_filename filename;
              INCLUDED []
            end else
              INCLUDED (read_ocamlconf inc_filename)
          | _ -> raise Parsing.Parse_error
        end
    | token -> token
  in
  let ast =
    try
      BuildOCPParser.main trap_include lexbuf
    with Parsing.Parse_error ->
      BuildMisc.print_loc filename (Lexing.lexeme_start lexbuf);
      Printf.eprintf "Parse error\n%!";
      exit 2
  in
  close_in ic;
  StmtOption (OptionListSet("dirname", [dir])) ::  ast
