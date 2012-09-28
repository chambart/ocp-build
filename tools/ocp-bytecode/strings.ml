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

type t = {
  mutable strings : string list;
  mutable length : int;
}

let create _ = { strings = []; length = 0 }

let add_immutable_string b s =
  b.strings <- s :: b.strings;
  b.length <- b.length + String.length s

let add_char b c =
  add_immutable_string b (String.make 1 c)

(* TODO: could be moved to OcpString *)
let rec blit_strings buf pos list =
  match list with
      [] -> ()
    | s :: tail ->
      let len = String.length s in
      String.blit s 0 buf pos len;
      blit_strings buf (pos+len) tail

let contents b =
  match b.strings with
      [] -> ""
    | [s] -> s
    | ss ->
      let rev = List.rev ss in
      let s = String.create b.length in
      blit_strings s 0 rev;
      b.strings <- [s];
      s

let length b = b.length

let printf b fmt =
  Printf.kprintf (fun s -> add_immutable_string b s) fmt
