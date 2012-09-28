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

type t

(*
val read : (string -> t)
*)
let read string = assert false

(*
val write : (string -> (t -> unit))
*)
let write string t = assert false



let cmxa_magic_number_001 = "Caml1999Z001" (* csl 1.06 - csl 1.07 *)
let cmxa_magic_number_002 = "Caml1999Z002" (* csl 1.10 - csl 1.15 *)
let cmxa_magic_number_003 = "Caml1999Z003" (* 1.00 - 1.03 *)
let cmxa_magic_number_004 = "Caml1999Z004" (* 1.04 - 1.05 *)
let cmxa_magic_number_005 = "Caml1999Z005" (* 1.06 - 1.07 *)
let cmxa_magic_number_006 = "Caml1999Z006" (* 2.00 - 2.99 *)
let cmxa_magic_number_007 = "Caml1999Z007" (* 3.00 - 3.03-alpha *)
let cmxa_magic_number_008 = "Caml1999Z008" (* 3.04 - 3.05 *)
let cmxa_magic_number_009 = "Caml1999Z009" (* 3.06 - 3.07 *)
let cmxa_magic_number_010 = "Caml1999Z010" (* 3.08 - 3.12.1 *)
