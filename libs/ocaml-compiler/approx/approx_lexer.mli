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

module Make(Tokens : Approx_tokens.Sig) : sig

  val init : unit -> unit
  val token : Lexing.lexbuf -> Tokens.token
  val token_with_comments: Lexing.lexbuf -> Tokens.token
  val token_pos : Lexing.lexbuf -> Tokens.token * (int * int)
  val comments : unit -> (int * int) list
  val token_locs : Lexing.lexbuf ->
    Tokens.token * (Lexing.position * Lexing.position)
  val token_locs_and_comments : Lexing.lexbuf ->
    Tokens.token * (Lexing.position * Lexing.position)
  val tokens_of_file :
    string -> (Tokens.token * (int * int)) list
  val tokens_of_string :
    string -> Tokens.token list
  val tokens_with_loc_of_string :
    string -> (Tokens.token * (int * int)) list
  val lines : unit -> (int * int) list
  val string_of_token : Tokens.token -> string
end

include Approx_tokens.Sig

val string_of_token : token -> string

val init : unit -> unit
val token : Lexing.lexbuf -> token
val token_with_comments: Lexing.lexbuf -> token
val token_pos : Lexing.lexbuf -> token * (int * int)
val comments : unit -> (int * int) list
val token_locs : Lexing.lexbuf ->
  token * (Lexing.position * Lexing.position)
val token_locs_and_comments : Lexing.lexbuf ->
  token * (Lexing.position * Lexing.position)
val tokens_of_file : string -> (token * (int * int)) list
val tokens_of_string : string -> token list
  val tokens_with_loc_of_string :
    string -> (token * (int * int)) list
val lines : unit -> (int * int) list
