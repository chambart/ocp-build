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

(* Entry points in the parser *)

val implementation : Lexing.lexbuf -> Parsetree.structure
val interface : Lexing.lexbuf -> Parsetree.signature
val toplevel_phrase : Lexing.lexbuf -> Parsetree.toplevel_phrase
val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list

module Lexer : sig

val init : unit -> unit
val token : Lexing.lexbuf -> Approx_parser.token
val comments : unit -> (int * int) list
val token_locs : Lexing.lexbuf ->
  Approx_parser.token * (Lexing.position * Lexing.position)
val token_locs_and_comments : Lexing.lexbuf ->
  Approx_parser.token * (Lexing.position * Lexing.position)


end
