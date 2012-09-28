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

let get_byte s pos = int_of_char s.[pos], pos+1

let buf_byte b n = Buffer.add_char b (char_of_int (n land 0xff))

let str_byte s pos n =  s.[pos] <- char_of_int (n land 0xff)

module LittleEndian = struct

let get_uint s pos =
  let b4,pos = get_byte s pos in
  let b3,pos = get_byte s pos in
  let b2,pos = get_byte s pos in
  let b1,pos = get_byte s pos in
  (b1 lsl 24) + (b2 lsl 16) + (b3 lsl 8) + b4

let str_uint s pos n =
  str_byte s pos n;
  str_byte s (pos+1) (n lsr 8);
  str_byte s (pos+2) (n lsr 16);
  str_byte s (pos+3) (n lsr 24)

let get_sint s pos =
  let b4,pos = get_byte s pos in
  let b3,pos = get_byte s pos in
  let b2,pos = get_byte s pos in
  let b1,pos = get_byte s pos in
  let b1' = if b1 >= 128 then b1-256 else b1 in
  (b1' lsl 24) + (b2 lsl 16) + (b3 lsl 8) + b4

let buf_byte b n = Buffer.add_char b (char_of_int (n land 0xff))

let buf_uint b n =
  buf_byte b n;
  buf_byte b (n lsr 8);
  buf_byte b (n lsr 16);
  buf_byte b (n lsr 24);
  ()

end

module BigEndian = struct

let get_uint s pos =
  let b1,pos = get_byte s pos in
  let b2,pos = get_byte s pos in
  let b3,pos = get_byte s pos in
  let b4,pos = get_byte s pos in
  (b1 lsl 24) + (b2 lsl 16) + (b3 lsl 8) + b4

let get_sint s pos =
  let b1,pos = get_byte s pos in
  let b1' = if b1 >= 128 then b1-256 else b1 in
  let b2,pos = get_byte s pos in
  let b3,pos = get_byte s pos in
  let b4,pos = get_byte s pos in
  (b1' lsl 24) + (b2 lsl 16) + (b3 lsl 8) + b4

let buf_uint b n =
  buf_byte b (n lsr 24);
  buf_byte b (n lsr 16);
  buf_byte b (n lsr 8);
  buf_byte b n

end
